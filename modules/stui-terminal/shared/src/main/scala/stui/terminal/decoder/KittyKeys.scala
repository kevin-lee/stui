package stui.terminal.decoder

import cats.syntax.all.*
import stui.core.event.{KeyCode, MediaKey, ModifierKey}

/** The functional keys of the kitty keyboard protocol that live in the Unicode Private Use Area (the specification's "Functional key
  * definitions" table, design doc 7.5, M3d). Keypad keys decode to their plain equivalents (the specification's own legacy rule, the
  * digits and operators as characters) because the event model has no keypad flag (design doc 6.2), so they have no encoding back;
  * every other key here round-trips through [[codeOf]].
  *
  * @author Kevin Lee
  * @since 2026-09-27
  */
private[decoder] object KittyKeys {

  /** The first Private Use Area code point. */
  val First: Int = 57344

  /** The last Private Use Area code point. */
  val Last: Int = 63743

  private val F13: Int = 57376

  private val F35: Int = 57398

  /** The key of a Private Use Area key code, `None` for a code the table does not name. */
  def keyCodeOf(code: Int): Option[KeyCode] =
    if (code >= F13 && code <= F35) KeyCode.fFrom(code - F13 + 13).toOption
    else fixed(code).orElse(media(code).map(KeyCode.media)).orElse(modifier(code).map(KeyCode.modifier))

  private def fixed(code: Int): Option[KeyCode] = code match {
    case 57358 => KeyCode.CapsLock.some
    case 57359 => KeyCode.ScrollLock.some
    case 57360 => KeyCode.NumLock.some
    case 57361 => KeyCode.PrintScreen.some
    case 57362 => KeyCode.Pause.some
    case 57363 => KeyCode.Menu.some
    case 57399 => KeyCode.char('0').some
    case 57400 => KeyCode.char('1').some
    case 57401 => KeyCode.char('2').some
    case 57402 => KeyCode.char('3').some
    case 57403 => KeyCode.char('4').some
    case 57404 => KeyCode.char('5').some
    case 57405 => KeyCode.char('6').some
    case 57406 => KeyCode.char('7').some
    case 57407 => KeyCode.char('8').some
    case 57408 => KeyCode.char('9').some
    case 57409 => KeyCode.char('.').some
    case 57410 => KeyCode.char('/').some
    case 57411 => KeyCode.char('*').some
    case 57412 => KeyCode.char('-').some
    case 57413 => KeyCode.char('+').some
    case 57414 => KeyCode.Enter.some
    case 57415 => KeyCode.char('=').some
    case 57416 => KeyCode.char(',').some
    case 57417 => KeyCode.Left.some
    case 57418 => KeyCode.Right.some
    case 57419 => KeyCode.Up.some
    case 57420 => KeyCode.Down.some
    case 57421 => KeyCode.PageUp.some
    case 57422 => KeyCode.PageDown.some
    case 57423 => KeyCode.Home.some
    case 57424 => KeyCode.End.some
    case 57425 => KeyCode.Insert.some
    case 57426 => KeyCode.Delete.some
    case 57427 => KeyCode.KeypadBegin.some
    case _ => none[KeyCode]
  }

  private def media(code: Int): Option[MediaKey] = code match {
    case 57428 => MediaKey.Play.some
    case 57429 => MediaKey.Pause.some
    case 57430 => MediaKey.PlayPause.some
    case 57431 => MediaKey.Reverse.some
    case 57432 => MediaKey.Stop.some
    case 57433 => MediaKey.FastForward.some
    case 57434 => MediaKey.Rewind.some
    case 57435 => MediaKey.TrackNext.some
    case 57436 => MediaKey.TrackPrevious.some
    case 57437 => MediaKey.Record.some
    case 57438 => MediaKey.LowerVolume.some
    case 57439 => MediaKey.RaiseVolume.some
    case 57440 => MediaKey.MuteVolume.some
    case _ => none[MediaKey]
  }

  private def modifier(code: Int): Option[ModifierKey] = code match {
    case 57441 => ModifierKey.LeftShift.some
    case 57442 => ModifierKey.LeftControl.some
    case 57443 => ModifierKey.LeftAlt.some
    case 57444 => ModifierKey.LeftSuper.some
    case 57445 => ModifierKey.LeftHyper.some
    case 57446 => ModifierKey.LeftMeta.some
    case 57447 => ModifierKey.RightShift.some
    case 57448 => ModifierKey.RightControl.some
    case 57449 => ModifierKey.RightAlt.some
    case 57450 => ModifierKey.RightSuper.some
    case 57451 => ModifierKey.RightHyper.some
    case 57452 => ModifierKey.RightMeta.some
    case 57453 => ModifierKey.IsoLevel3Shift.some
    case 57454 => ModifierKey.IsoLevel5Shift.some
    case _ => none[ModifierKey]
  }

  /** The Private Use Area code of a key that has one and no legacy form: the lock keys, Print Screen, Pause, Menu, Keypad Begin, F13
    * to F35, the media keys, and the modifier keys.
    */
  def codeOf(keyCode: KeyCode): Option[Int] = keyCode match {
    case KeyCode.CapsLock => 57358.some
    case KeyCode.ScrollLock => 57359.some
    case KeyCode.NumLock => 57360.some
    case KeyCode.PrintScreen => 57361.some
    case KeyCode.Pause => 57362.some
    case KeyCode.Menu => 57363.some
    case KeyCode.KeypadBegin => 57427.some
    case KeyCode.F(n) => Option.when(n.value >= 13)(F13 + n.value - 13)
    case KeyCode.Media(key) => mediaCode(key).some
    case KeyCode.Modifier(key) => modifierCode(key).some
    case KeyCode.Char(_) | KeyCode.Backspace | KeyCode.Enter | KeyCode.Left | KeyCode.Right | KeyCode.Up | KeyCode.Down | KeyCode.Home |
        KeyCode.End | KeyCode.PageUp | KeyCode.PageDown | KeyCode.Tab | KeyCode.BackTab | KeyCode.Delete | KeyCode.Insert |
        KeyCode.Escape =>
      none[Int]
  }

  private def mediaCode(key: MediaKey): Int = key match {
    case MediaKey.Play => 57428
    case MediaKey.Pause => 57429
    case MediaKey.PlayPause => 57430
    case MediaKey.Reverse => 57431
    case MediaKey.Stop => 57432
    case MediaKey.FastForward => 57433
    case MediaKey.Rewind => 57434
    case MediaKey.TrackNext => 57435
    case MediaKey.TrackPrevious => 57436
    case MediaKey.Record => 57437
    case MediaKey.LowerVolume => 57438
    case MediaKey.RaiseVolume => 57439
    case MediaKey.MuteVolume => 57440
  }

  private def modifierCode(key: ModifierKey): Int = key match {
    case ModifierKey.LeftShift => 57441
    case ModifierKey.LeftControl => 57442
    case ModifierKey.LeftAlt => 57443
    case ModifierKey.LeftSuper => 57444
    case ModifierKey.LeftHyper => 57445
    case ModifierKey.LeftMeta => 57446
    case ModifierKey.RightShift => 57447
    case ModifierKey.RightControl => 57448
    case ModifierKey.RightAlt => 57449
    case ModifierKey.RightSuper => 57450
    case ModifierKey.RightHyper => 57451
    case ModifierKey.RightMeta => 57452
    case ModifierKey.IsoLevel3Shift => 57453
    case ModifierKey.IsoLevel5Shift => 57454
  }

}
