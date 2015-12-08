package wirth

import shapeless.HNil
import fastparse.all._

/*
 A 'Free' Parser of T - something which can be interpreted
 (under some context) as a parser of T
 */

trait FParser[T] {
  def parser() : Parser[T]
}

object FParser {
  implicit def hnil: FParser[HNil] = new FParser[HNil] {
    def parser(): Parser[HNil] = Pass.map(_ => HNil)
  }
}
