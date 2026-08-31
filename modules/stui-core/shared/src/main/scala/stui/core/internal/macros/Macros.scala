package stui.core.internal.macros

import scala.quoted.*

object Macros {

  def exprToStringImpl[A](expr: Expr[A])(using Quotes): Expr[String] = {
    Expr(exprToString(expr))
  }

  def exprToString[A](expr: Expr[A])(using Quotes): String = {
    import quotes.reflect.*

    @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
    def unwrap(term: Term): Term = term match {
      case Inlined(_, _, t) =>
        // println(s"Inlined: $t")
        unwrap(t)
      case Block(_, t) =>
        // println(s"Block: $t")
        unwrap(t)
      case TypeApply(t, _) =>
        // println(s"TypeApply: $t")
        unwrap(t)
      case Select(t, _) =>
        // println(s"Select: $t")
        unwrap(t)
      case Typed(t, _) =>
        // println(s"Typed: $t")
        unwrap(t)
      case _ => term
    }

    unwrap(expr.asTerm) match {
      case Literal(StringConstant(value)) => value

      case Literal(LongConstant(value)) => value.toString + "L"

      case Literal(IntConstant(value)) => value.toString

      case Literal(FloatConstant(value)) => value.toString + "f"

      case Literal(DoubleConstant(value)) => value.toString + "d"

      case Literal(ShortConstant(value)) => value.toString

      case Literal(ByteConstant(value)) => value.toString

      case Literal(CharConstant(value)) => value.toString

      case Literal(BooleanConstant(value)) => value.toString

      case Literal(UnitConstant()) => "()"

      case Literal(NullConstant()) => "null"

      case _ =>
        report.errorAndAbort(
          s"Expected a constant string for the value but got: ${expr.show}\n" +
            s"Term: ${expr.asTerm}\n" +
            s"Unwrapped: ${unwrap(expr.asTerm)}"
        )
    }
  }

}
