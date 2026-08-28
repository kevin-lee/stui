package stui.terminal.ansi

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import refined4s.types.numeric.NonNegInt
import stui.core.capability.{Capabilities, ColorProfile}
import stui.core.geometry.Size
import stui.core.style.{CellStyle, Color, Style, UnderlineStyle}
import stui.testkit.{Assertions, TerminalModel}
import stui.testkit.TerminalModel.{QuirkProfile, Screen}
import stui.testkit.gen.{CapabilityGens, StyleGens}

/** Style deltas and normalisation, and the agreement between what `Sgr` emits and what the oracle reads back.
  *
  * @author Kevin Lee
  * @since 2026-08-29
  */
object SgrSpec extends Properties {

  private val lossless: Capabilities = Capabilities.lossless

  private val plain: Capabilities = Capabilities.conservative.withColors(ColorProfile.Truecolor)

  private val csi: String = Sequences.Csi

  private def styled(patch: Style): CellStyle = CellStyle.default.patch(patch)

  override def tests: List[Test] = List(
    property("delta from a style to itself is empty", testSelf),
    example("a red foreground", Assertions.eqv(Sgr.delta(CellStyle.default, styled(Style.empty.withFg(Color.Red)), lossless), csi + "31m")),
    example(
      "a bright background",
      Assertions.eqv(Sgr.delta(CellStyle.default, styled(Style.empty.withBg(Color.LightBlue)), lossless), csi + "104m"),
    ),
    example(
      "bold and dim to dim only re-adds dim",
      Assertions.eqv(Sgr.delta(styled(Style.empty.bold.dim), styled(Style.empty.dim), lossless), csi + "22;2m"),
    ),
    example("bold to dim", Assertions.eqv(Sgr.delta(styled(Style.empty.bold), styled(Style.empty.dim), lossless), csi + "22;2m")),
    example(
      "curly with extended underlines",
      Assertions.eqv(
        Sgr.delta(CellStyle.default, Sgr.normalise(lossless, styled(Style.empty.withUnderline(UnderlineStyle.Curly))), lossless),
        csi + "4:3m",
      ),
    ),
    example(
      "curly without extended underlines is a plain 4",
      Assertions
        .eqv(Sgr.delta(CellStyle.default, Sgr.normalise(plain, styled(Style.empty.withUnderline(UnderlineStyle.Curly))), plain), csi + "4m"),
    ),
    example("single to curly without extended underlines emits nothing", testFoldedUnderline),
    example("an underline colour under Ansi16 is dropped", testUnderlineColorDropped),
    example(
      "an underline colour under truecolour with extended underlines",
      Assertions.eqv(
        Sgr.delta(CellStyle.default, Sgr.normalise(lossless, styled(Style.empty.withUnderlineColor(Color.rgb(1, 2, 3)))), lossless),
        csi + "58;2;1;2;3m",
      ),
    ),
    example("Mono emits attributes only", testMono),
    example(
      "indexed colours",
      Assertions.eqv(
        Sgr.delta(CellStyle.default, styled(Style.empty.withFg(Color.indexed(200)).withBg(Color.indexed(7))), lossless),
        csi + "38;5;200;48;5;7m",
      ),
    ),
    example(
      "the sixteen indices",
      Result.all(
        List(
          Assertions.eqv(Sgr.colorIndex(Color.Black), 0.some),
          Assertions.eqv(Sgr.colorIndex(Color.White), 15.some),
          Assertions.eqv(Sgr.colorIndex(Color.Reset), none[Int]),
          Assertions.eqv(Sgr.colorIndex(Color.rgb(1, 2, 3)), none[Int]),
        )
      ),
    ),
    property("normalise is idempotent", testNormaliseIdempotent),
    property("the oracle reads back what delta emits", testOracle),
  )

  def testSelf: Property =
    for {
      s <- StyleGens.cellStyle.forAll
      c <- CapabilityGens.capabilities.forAll
    } yield Assertions.eqv(Sgr.delta(s, s, c), "")

  def testFoldedUnderline: Result = {
    val from = Sgr.normalise(plain, styled(Style.empty.withUnderline(UnderlineStyle.Single)))
    val to   = Sgr.normalise(plain, styled(Style.empty.withUnderline(UnderlineStyle.Curly)))
    Assertions.eqv(Sgr.delta(from, to, plain), "")
  }

  def testUnderlineColorDropped: Result = {
    val caps = Capabilities.conservative.copy(extendedUnderline = true)
    Assertions.eqv(Sgr.normalise(caps, styled(Style.empty.withUnderlineColor(Color.Red))), CellStyle.default)
  }

  def testMono: Result = {
    val caps = Capabilities.conservative.withColors(ColorProfile.Mono)
    val to   = Sgr.normalise(caps, styled(Style.empty.withFg(Color.Red).withBg(Color.Blue).bold))
    Assertions.eqv(Sgr.delta(CellStyle.default, to, caps), csi + "1m")
  }

  def testNormaliseIdempotent: Property =
    for {
      s <- StyleGens.cellStyle.forAll
      c <- CapabilityGens.capabilities.forAll
    } yield Assertions.eqv(Sgr.normalise(c, Sgr.normalise(c, s)), Sgr.normalise(c, s))

  def testOracle: Property =
    for {
      s <- StyleGens.cellStyle.forAll
      c <- CapabilityGens.capabilities.forAll
    } yield {
      val normalised = Sgr.normalise(c, s)
      val screen     = Screen.blank(QuirkProfile.default, Size(NonNegInt(1), NonNegInt(1)))
      TerminalModel.interpret(QuirkProfile.default, screen, Sgr.delta(CellStyle.default, normalised, c)) match {
        case Right(after) => Assertions.eqv(after.style, normalised)
        case Left(error) => Result.failure.log(error.show)
      }
    }

}
