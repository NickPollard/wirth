package wirth

import fastparse.core.Parser
import org.scalatest.FreeSpec
import shapeless.the
import Parsable.string._

sealed trait Source
case class Http(url: String) extends Source
case class DB(table: String) extends Source
case class Fallback(first: Source, second: Source) extends Source

class SourceTest extends FreeSpec {
  import Parsable.string._

  val parsable : Parsable[Source] = the[Parsable[Source]]

  "String" in {
    val prsr = the[Parsable[String]].parser("(", ",", ")")
    assert( prsr.parse("\"test\"").get.value === "test")
  }

  "Source" - {
    val prsr = parsable.parser("(", ",", ")")
    "Http" in {
      assert(prsr.parse("Http(\"http://www.example.com\")").get.value === Http("http://www.example.com"))
    }
    "DB" in {
      assert(prsr.parse("DB(\"source_table\")").get.value === DB("source_table"))
    }
    "Fallback" in {
      assert(prsr.parse("Fallback(Http(\"http://www.example.com\"),DB(\"source_table\"))").get.value === Fallback(Http("http://www.example.com"), DB("source_table")))
    }
  }
}