package wirth

import org.scalatest.FreeSpec
import shapeless.the
import Parsable.string._

/*
  EBNF:
    leaf = "Leaf(" number ")"
    node = leaf; leaf
    tree = leaf | node
 */

sealed trait Tree {
  def sum: Int
}
case class Leaf(number: Int) extends Tree {
  def sum = number
}
// TODO - this doesn't work as it's left recursive!
case class Node(left: Tree, right: Tree) extends Tree {
  def sum = left.sum + right.sum
}

object Tree {
  val simple = "Leaf(1,)"
//  val example = "Node(Leaf(1,),Leaf(2,),)"
  val example = "Node(Leaf(1,),Node(Node(Leaf(2,),Leaf(3,),),Leaf(4,),),)"
}

case class Test(a: Int)

class Example extends FreeSpec {
  import Parsable.int._

  "Int" in {
    //implicit val ip = Parsable.int.p
    val prsr = the[Parsable[Int]].parser()
    assert( prsr.parse("1").get.value === 1)
  }
  "Test" in {
    val prsr = the[Parsable[Test]].parser()
    assert( prsr.parse("1,").get.value === Test(1))
  }
  "Leaf" in {
    val prsr = the[Parsable[Leaf]].parser()
    assert( prsr.parse("1,").get.value === Leaf(1))
  }
  "Simple tree" in {
    val prsr = the[Parsable[Tree]].parser()
    assert(prsr.parse(Tree.simple).get.value.sum === 1)
  }
  "Example" in {
    val prsr = the[Parsable[Tree]].parser()
    assert(prsr.parse(Tree.example).get.value.sum === 10)
  }
}