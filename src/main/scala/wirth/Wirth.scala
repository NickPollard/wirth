package wirth

object Wirth {
  def parserOf[T](open: String, sep: String, close: String)(implicit p:Parsable[T]) = p.parser(open, sep, close)
}
