package stui.core.capability

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*
import stui.testkit.Assertions
import stui.testkit.gen.CapabilityGens
import stui.testkit.laws.MonoidLaws

/** The capabilities patch (design doc 7.3, decision D15): the monoid laws, the patch action, the identity, the right bias, and the
  * no-upgrade law of the three-layer merge.
  *
  * @author Kevin Lee
  * @since 2026-08-30
  */
object CapabilitiesPatchSpec extends Properties {

  override def tests: List[Test] =
    MonoidLaws.laws("CapabilitiesPatch", CapabilityGens.capabilitiesPatch) ++ List(
      property("patching twice is patching with the combined patch (the action law)", testAction),
      property("the empty patch changes nothing", testIdentity),
      property("no field upgrades except through a patch that names it (the no-upgrade law)", testNoUpgrade),
      example("the later patch wins where both say something", testRightBias),
      example("every builder says exactly its field", testBuilders),
    )

  def testAction: Property =
    for {
      capabilities <- CapabilityGens.capabilities.forAll
      a            <- CapabilityGens.capabilitiesPatch.forAll
      b            <- CapabilityGens.capabilitiesPatch.forAll
    } yield Assertions.eqv(capabilities.patched(a).patched(b), capabilities.patched(a |+| b))

  def testIdentity: Property =
    CapabilityGens.capabilities.forAll.map(capabilities => Assertions.eqv(capabilities.patched(CapabilitiesPatch.empty), capabilities))

  def testNoUpgrade: Property =
    for {
      env <- CapabilityGens.env.forAll
      a   <- CapabilityGens.capabilitiesPatch.forAll
      b   <- CapabilityGens.capabilitiesPatch.forAll
    } yield {
      val base                                                            = Capabilities.fromEnv(env)
      val combined                                                        = a |+| b
      val merged                                                          = Capabilities.merge(base, a, b)
      def field(name: String)(unchanged: Boolean, named: Boolean): Result = Result.assert(named || unchanged).log(name)
      Result.all(
        List(
          field("colors")(merged.colors === base.colors, combined.colors.isDefined),
          field("syncOutput")(merged.syncOutput === base.syncOutput, combined.syncOutput.isDefined),
          field("kittyKeyboard")(merged.kittyKeyboard === base.kittyKeyboard, combined.kittyKeyboard.isDefined),
          field("sgrMouse")(merged.sgrMouse === base.sgrMouse, combined.sgrMouse.isDefined),
          field("focusEvents")(merged.focusEvents === base.focusEvents, combined.focusEvents.isDefined),
          field("scrollRegionsSafe")(merged.scrollRegionsSafe === base.scrollRegionsSafe, combined.scrollRegionsSafe.isDefined),
          field("extendedUnderline")(merged.extendedUnderline === base.extendedUnderline, combined.extendedUnderline.isDefined),
          field("multiplexer")(merged.multiplexer === base.multiplexer, combined.multiplexer.isDefined),
          field("ssh")(merged.ssh === base.ssh, combined.ssh.isDefined),
          field("glyphs")(merged.glyphs === base.glyphs, combined.glyphs.isDefined),
          field("ambiguousWide")(merged.ambiguousWide === base.ambiguousWide, combined.ambiguousWide.isDefined),
          field("vs16Width")(merged.vs16Width === base.vs16Width, combined.vs16Width.isDefined),
        )
      )
    }

  def testRightBias: Result = {
    val a = CapabilitiesPatch.empty.withColors(ColorProfile.Ansi256).withSyncOutput(false)
    val b = CapabilitiesPatch.empty.withColors(ColorProfile.Truecolor)
    val c = a |+| b
    Result.all(
      List(
        Assertions.eqv(c.colors, (ColorProfile.Truecolor: ColorProfile).some),
        Assertions.eqv(c.syncOutput, false.some),
      )
    )
  }

  def testBuilders: Result = {
    val builders = List(
      CapabilitiesPatch.empty.withColors(ColorProfile.Mono),
      CapabilitiesPatch.empty.withSyncOutput(true),
      CapabilitiesPatch.empty.withKittyKeyboard(true),
      CapabilitiesPatch.empty.withSgrMouse(true),
      CapabilitiesPatch.empty.withFocusEvents(true),
      CapabilitiesPatch.empty.withScrollRegionsSafe(true),
      CapabilitiesPatch.empty.withExtendedUnderline(true),
      CapabilitiesPatch.empty.withMultiplexer(Multiplexer.Tmux),
      CapabilitiesPatch.empty.withSsh(true),
      CapabilitiesPatch.empty.withGlyphs(GlyphSet.Ascii),
      CapabilitiesPatch.empty.withAmbiguousWide(true),
      CapabilitiesPatch.empty.withVs16Width(stui.core.buffer.GlyphWidth.One),
    )
    /* each builder must produce a patch that says exactly one field */
    val counts   = builders.map { patch =>
      List(
        patch.colors.isDefined,
        patch.syncOutput.isDefined,
        patch.kittyKeyboard.isDefined,
        patch.sgrMouse.isDefined,
        patch.focusEvents.isDefined,
        patch.scrollRegionsSafe.isDefined,
        patch.extendedUnderline.isDefined,
        patch.multiplexer.isDefined,
        patch.ssh.isDefined,
        patch.glyphs.isDefined,
        patch.ambiguousWide.isDefined,
        patch.vs16Width.isDefined,
      ).count(identity)
    }
    Assertions.eqv(counts, List.fill(12)(1))
  }

}
