package stui.unicode.internal

/** Type-safe equality on `Int` for the table and segmentation hot paths.
  *
  * wartremover's `Equals` wart rejects `==` on every type, and a generic `===` would box the operands, so this is the one place where the
  * primitive comparison is wrapped. The methods are `inline`, so call sites pay nothing.
  *
  * @author Kevin Lee
  * @since 2026-08-22
  */
@SuppressWarnings(Array("org.wartremover.warts.Equals")) // reason: the single wrapped primitive comparison, inlined at every call site
private[unicode] object IntOps {

  extension (a: Int) {

    inline def ===(b: Int): Boolean = a == b

    inline def !==(b: Int): Boolean = a != b

  }

}
