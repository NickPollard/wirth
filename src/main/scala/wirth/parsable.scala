package wirth

import shapeless._
import fastparse.WhitespaceApi
import shapeless.labelled.{FieldType, field}

/*
 A Parsable of T - something which can be interpreted (under some context) as a parser of T

 Wirth constructs a Parsable of a compound type from a shapeless product/coproduct derivation,
 with which you may then create a parser of the required form
 */

/*
object WhiteSpace {
  val api = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(" ".rep)
  }
}

import fastparse.noApi._
import WhiteSpace.api._
*/
import fastparse.all._

trait Parsable[T] {
  def parser(open: String, sep: String, close: String) : Parser[T]
}

trait ParsableSimple {
  def const[T](p: Parser[T]) = new Parsable[T] { def parser(open: String, sep: String, close: String) = p }
}
trait ParsableProduct extends ParsableSimple {
  implicit def project[T,U](implicit ev: LabelledGeneric.Aux[T,U], p: Lazy[Parsable[U]]) : Parsable[T] = new Parsable[T] {
    def parser(open: String, sep: String, close: String) : Parser[T] = p.value.parser(open, sep, close) map ev.from
  }
  implicit def hnil: Parsable[HNil] = const[HNil](Pass map (_ => HNil))
  implicit def hcons[K <: Symbol, H, T <: HList](implicit head: Lazy[Parsable[H]],
                                                          tail: Lazy[Parsable[T]]): Parsable[FieldType[K,H] :: T] = new Parsable[FieldType[K,H] :: T] {
    def parser(open: String, sep: String, close: String) = head.value.parser(open, sep, close) ~ P(sep) ~ tail.value.parser(open, sep, close) map { case (h,t) => field[K](h) :: t }
  }
}

trait ParsableProductHNil extends ParsableProduct {
  implicit def hconsNil[K <: Symbol, H](implicit head: Lazy[Parsable[H]]): Parsable[FieldType[K,H] :: HNil] = new Parsable[FieldType[K,H] :: HNil] {
    def parser(open: String, sep: String, close: String) = head.value.parser(open, sep, close) map ( field[K](_) :: HNil )
  }
}

trait ParsableCoproduct extends ParsableProductHNil {
  implicit def cnil : Parsable[CNil] = const[CNil](Fail)
  implicit def ccons[K <: Symbol, H, T <: Coproduct](implicit head: Lazy[Parsable[H]],
                                                              key: Witness.Aux[K] ,
                                                              tail: Lazy[Parsable[T]]): Parsable[FieldType[K,H] :+: T] = new Parsable[FieldType[K,H] :+: T] {
    def parser(open: String, sep: String, close: String) = P(P(key.value.name) ~ P(open) ~ head.value.parser(open, sep, close).map(l => Inl(field[K](l))) ~ P(close)) | P(tail.value.parser(open, sep, close).map(Inr(_)))
  }
}

object Parsable extends ParsableCoproduct {
  def apply[T](implicit p: Parsable[T]): Parsable[T] = p

  object string {
    implicit val p : Parsable[String] = Parsable.const(P("\"") ~ CharsWhile(_ != '"').! ~ P("\""))
  }
  object int {
    private val digit = CharIn('0'to'9').!
    implicit val p : Parsable[Int] = Parsable.const[Int]((digit rep 1).!.map((s:String) => s.toInt))
  }
}
