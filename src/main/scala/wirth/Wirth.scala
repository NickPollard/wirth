package wirth

object Wirth {
  def parserOf[T](implicit p:Parsable[T]) = p.parser()
}
