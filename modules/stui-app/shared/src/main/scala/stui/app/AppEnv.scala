package stui.app

import cats.{Eq, Hash, Show}
import cats.derived.strict.*
import stui.core.capability.Capabilities
import stui.core.geometry.Size
import stui.core.spi.ScreenMode

/** What the runtime hands `StuiApp.init` (design doc 10, M3b): the session's capabilities, its screen mode, and the viewport size at
  * the first frame. Later sizes arrive as `Event.Resize`, so an application keeps what it needs of this value in its model.
  *
  * @author Kevin Lee
  * @since 2026-09-05
  */
final case class AppEnv(capabilities: Capabilities, screenMode: ScreenMode, viewport: Size) derives Eq, Show, Hash

object AppEnv {

  extension (env: AppEnv) {

    /** True in inline mode (the UI occupies the last rows of the normal screen, design doc 7.2). */
    def isInline: Boolean = env.screenMode match {
      case ScreenMode.AlternateScreen => false
      case ScreenMode.Inline(_) => true
    }

  }

}
