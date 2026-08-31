package stui.terminal

import stui.core.geometry.Size
import stui.core.internal.NonNegInts
import stui.core.spi.{ScreenMode, TerminalOptions}

/** The `Resize` payload mapping of the D12 under-run rule (design doc 7.2): the terminal size on the alternate screen, the width with
  * the height clamped to the requested inline height in inline mode. Shared because every platform's event source delivers the
  * effective viewport size while deduplicating on the terminal size.
  *
  * @author Kevin Lee
  * @since 2026-08-31
  */
object EffectiveSize {

  /** The mapping from the reported terminal size to the effective viewport size for the options' screen mode. */
  def of(options: TerminalOptions): Size => Size = options.screenMode match {
    case ScreenMode.AlternateScreen => identity
    case ScreenMode.Inline(height) =>
      size =>
        Size(
          size.width,
          NonNegInts.min(NonNegInts.clamp(height.value.toLong), size.height),
        )
  }

}
