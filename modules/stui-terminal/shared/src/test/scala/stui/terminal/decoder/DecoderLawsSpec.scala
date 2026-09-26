package stui.terminal.decoder

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.core.event.{Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers}
import stui.terminal.kitty.KittyFlags
import stui.testkit.Assertions
import stui.unicode.internal.IntOps.*

/** The decoder laws (design doc 7.5 and 12): the round trip through the encoder, chunk-split invariance, determinism, the state-size
  * bound, fuzzing, ticks, and printable text, and for the kitty keyboard protocol (M3d) the per-mode round trip, the text law, the
  * release-naming law, the press-only filter, the stall rule, the same bounds over pushed states, and the control-sequence overflow.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object DecoderLawsSpec extends Properties {

  private val bound: Int =
    DecoderLimits.MaxPaste + DecoderLimits.MaxControlSequence + DecoderLimits.MaxStringSequence + DecoderLimits.MaxUtf8Pending

  override def tests: List[Test] = List(
    property("an encoded event decodes back to itself", testRoundTrip),
    property("splitting the bytes into chunks does not change the events", testSplits),
    property("splitting with a trailing tick does not change the events", testSplitsWithTick),
    property("decoding is deterministic", testDeterministic),
    property("the state stays bounded", testBounded),
    property("any input sequence decodes without failure and keeps every coordinate in range", testFuzz),
    property("a tick leaves nothing awaiting outside a paste", testTick),
    property("printable text decodes to one Char per character", testText),
    property("a cursor report never appears without the probe flag", testNoCprWithoutFlag),
    property("a kitty-encoded event decodes back to itself with the flags pushed", testKittyRoundTrip),
    property("a report's associated text decodes as the same text sent as UTF-8", testKittyText),
    property("a letter's release names the key its press named", testReleaseNaming),
    property("without EventTypes a release is dropped and a repeat is a press", testPressOnly),
    property("under the disambiguate flag a tick never yields Escape or re-read keys", testStall),
    property("kitty states stay bounded and deterministic with coordinates in range", testKittyBounded),
    property("a control sequence over the bound is consumed whole", testOverlong),
  )

  private val replacement: Char = '\uFFFD'

  private def press(c: Char): Event = Event.key(KeyEvent(KeyCode.Char(c), KeyModifiers.empty, KeyEventKind.Press))

  def testKittyRoundTrip: Property =
    for {
      flags <- DecoderGens.kittyFlags.forAll
      event <- DecoderGens.kittyEncodable(flags).forAll
    } yield Encoder.encodeKitty(flags, event) match {
      case Some(bytes) =>
        Assertions.eqv(
          Decoder.stepAll(DecoderState.initial.withKeyboard(flags), Vector(DecoderInput.bytes(bytes), DecoderInput.Tick)).events,
          Vector(event),
        )
      case None => Result.failure.log("not encodable")
    }

  def testKittyText: Property =
    for {
      state <- DecoderGens.kittyState.forAll
      key   <- Gen.element1(0, 13, 97, 111).forAll
      text  <- DecoderGens.reportText.forAll
    } yield {
      val report = "\u001b[" + key.toString + ";;" + text.map(_.toInt.toString).mkString(":") + "u"
      Assertions.eqv(
        Decoder.stepAll(state, Vector(DecoderInput.bytesOf(report), DecoderInput.Tick)).events,
        Decoder.step(DecoderState.initial, DecoderInput.bytesOf(text)).events,
      )
    }

  def testReleaseNaming: Property =
    for {
      flags <- Gen.element1(3, 7).map(KittyFlags.fromInt).forAll
      c     <- DecoderGens.letter.forAll
      shift <- Gen.boolean.forAll
      caps  <- Gen.boolean.forAll
    } yield {
      val state     = DecoderState.initial.withKeyboard(flags)
      val typed     = if (shift ^ caps) (c.toInt - 0x20).toChar else c
      val parameter = 1 + (if (shift) 1 else 0) + (if (caps) 64 else 0)
      val pressed   = Decoder.step(state, DecoderInput.bytesOf(typed.toString)).events
      val released  = Decoder.step(state, DecoderInput.bytesOf("\u001b[" + c.toInt.toString + ";" + parameter.toString + ":3u")).events
      (pressed, released) match {
        case (Vector(Event.Key(p)), Vector(Event.Key(r))) =>
          Result.all(
            List(
              Assertions.eqv(r.code, p.code),
              Assertions.eqv(r.modifiers, p.modifiers),
              Assertions.eqv(r.kind, KeyEventKind.Release),
            )
          )
        case (_, _) => Result.failure.log(s"press ${pressed.show}, release ${released.show}")
      }
    }

  def testPressOnly: Property =
    DecoderGens.kittyEncodable(KittyFlags.fromInt(3)).forAll.map { event =>
      val disambiguated = DecoderState.initial.withKeyboard(KittyFlags.Disambiguate)
      Encoder.encodeKitty(KittyFlags.fromInt(3), event) match {
        case Some(bytes) =>
          val events = Decoder.stepAll(disambiguated, Vector(DecoderInput.bytes(bytes), DecoderInput.Tick)).events
          event match {
            case Event.Key(KeyEvent(_, _, KeyEventKind.Release)) => Assertions.eqv(events, Vector.empty[Event])
            case Event.Key(KeyEvent(code, modifiers, KeyEventKind.Repeat)) =>
              Assertions.eqv(events, Vector(Event.key(KeyEvent(code, modifiers, KeyEventKind.Press))))
            case Event.Key(KeyEvent(_, _, KeyEventKind.Press)) | Event.Mouse(_) | Event.Resize(_) | Event.Paste(_) | Event.FocusGained |
                Event.FocusLost =>
              Assertions.eqv(events, Vector(event))
          }
        case None => Result.failure.log("not encodable")
      }
    }

  def testStall: Property =
    for {
      flags  <- DecoderGens.kittyFlags.forAll
      inputs <- DecoderGens.inputs.forAll
    } yield {
      val before          = Decoder.stepAll(DecoderState.initial.withKeyboard(flags), inputs).state
      val ticked          = Decoder.step(before, DecoderInput.Tick)
      val onlyReplacement = ticked.events match {
        case Vector() => true
        case Vector(Event.Key(KeyEvent(KeyCode.Char(c), _, KeyEventKind.Press))) => c === replacement
        case _ => false
      }
      Result.all(
        List(
          Result.assert(onlyReplacement).log(ticked.events.show),
          Result.assert(!ticked.state.awaiting).log(ticked.state.show),
        )
      )
    }

  def testKittyBounded: Property =
    for {
      state  <- DecoderGens.kittyState.forAll
      inputs <- DecoderGens.inputs.forAll
    } yield {
      val decoded = Decoder.stepAll(state, inputs)
      val inRange = decoded.events.forall {
        case Event.Mouse(mouse) =>
          mouse.position.x.value < DecoderLimits.MaxCoordinate && mouse.position.y.value < DecoderLimits.MaxCoordinate
        case Event.Key(_) | Event.Resize(_) | Event.Paste(_) | Event.FocusGained | Event.FocusLost => true
      }
      Result.all(
        List(
          Assertions.eqv(decoded, Decoder.stepAll(state, inputs)),
          Result.assert(decoded.state.bufferedBytes <= bound).log(s"buffered ${decoded.state.bufferedBytes.toString}"),
          Result.assert(inRange).log("coordinates"),
        )
      )
    }

  def testOverlong: Property =
    Gen.int(Range.linear(513, 1500)).forAll.map { pairs =>
      val decoded = Decoder.stepAll(
        DecoderState.initial,
        Vector(DecoderInput.bytesOf("\u001b[" + "1;" * pairs + "A"), DecoderInput.bytesOf("a")),
      )
      Result.all(List(Assertions.eqv(decoded.events, Vector(press('a'))), Assertions.eqv(decoded.state, DecoderState.initial)))
    }

  def testRoundTrip: Property =
    DecoderGens.encodable.forAll.map { event =>
      Encoder.encode(event) match {
        case Some(bytes) =>
          Decoder.stepAll(DecoderState.initial, Vector(DecoderInput.bytes(bytes), DecoderInput.Tick)) match {
            case Decoded(_, events, _) => Assertions.eqv(events, Vector(event))
          }
        case None => Result.failure.log("not encodable")
      }
    }

  def testSplits: Property =
    for {
      bytes  <- DecoderGens.chunk.forAll
      chunks <- DecoderGens.splits(bytes).forAll
    } yield Assertions.eqv(
      Decoder.stepAll(DecoderState.initial, chunks.map(DecoderInput.bytes)),
      Decoder.step(DecoderState.initial, DecoderInput.bytes(bytes)),
    )

  def testSplitsWithTick: Property =
    for {
      bytes  <- DecoderGens.chunk.forAll
      chunks <- DecoderGens.splits(bytes).forAll
    } yield Assertions.eqv(
      Decoder.stepAll(DecoderState.initial, chunks.map(DecoderInput.bytes) :+ DecoderInput.Tick),
      Decoder.stepAll(DecoderState.initial, Vector(DecoderInput.bytes(bytes), DecoderInput.Tick)),
    )

  def testDeterministic: Property =
    DecoderGens
      .inputs
      .forAll
      .map(inputs => Assertions.eqv(Decoder.stepAll(DecoderState.initial, inputs), Decoder.stepAll(DecoderState.initial, inputs)))

  def testBounded: Property =
    DecoderGens.inputs.forAll.map { inputs =>
      val state = Decoder.stepAll(DecoderState.initial, inputs).state
      Result.assert(state.bufferedBytes <= bound).log(s"buffered ${state.bufferedBytes.toString}")
    }

  def testFuzz: Property =
    DecoderGens.inputs.forAll.map { inputs =>
      Decoder.stepAll(DecoderState.initial, inputs) match {
        case Decoded(_, events, _) =>
          Result.all(events.toList.map {
            case Event.Mouse(mouse) =>
              Result.assert(mouse.position.x.value < DecoderLimits.MaxCoordinate && mouse.position.y.value < DecoderLimits.MaxCoordinate)
            case Event.Key(_) | Event.Resize(_) | Event.Paste(_) | Event.FocusGained | Event.FocusLost => Result.success
          })
      }
    }

  def testTick: Property =
    DecoderGens.inputs.forAll.map { inputs =>
      val before = Decoder.stepAll(DecoderState.initial, inputs).state
      val after  = Decoder.step(before, DecoderInput.Tick).state
      after.mode match {
        case DecoderMode.Paste(_, _) => Result.success
        case DecoderMode.Ground | DecoderMode.Escape | DecoderMode.EscapeIntermediate(_) | DecoderMode.Csi(_, _) | DecoderMode.Ss3 |
            DecoderMode.StringSeq(_, _, _, _) | DecoderMode.X10Mouse(_) =>
          Result.assert(!after.awaiting).log(after.show)
      }
    }

  def testText: Property =
    DecoderGens.printableText.forAll.map { text =>
      val events = Decoder.step(DecoderState.initial, DecoderInput.bytesOf(text)).events
      val chars  = events.map {
        case Event.Key(KeyEvent(KeyCode.Char(c), _, _)) => c.toInt.some
        case Event.Key(_) | Event.Mouse(_) | Event.Resize(_) | Event.Paste(_) | Event.FocusGained | Event.FocusLost => none[Int]
      }
      Result.all(
        List(
          Result.assert(events.length === text.length).log(s"${events.length.toString} events for ${text.length.toString} chars"),
          Assertions.eqv(chars, text.toVector.map(c => c.toInt.some)),
        )
      )
    }

  def testNoCprWithoutFlag: Property =
    DecoderGens.inputs.forAll.map { inputs =>
      val cprs = Decoder.stepAll(DecoderState.initial, inputs).replies.collect { case Reply.CursorPosition(position) => position }
      Result.assert(cprs.isEmpty).log(cprs.show)
    }

}
