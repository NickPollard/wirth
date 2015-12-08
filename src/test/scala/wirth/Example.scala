package wirth

import org.scalatest.FreeSpec
import shapeless.the

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
case class Node(left: Tree, right: Tree) extends Tree {
  def sum = left.sum + right.sum
}

object Tree {
  val example = "Node(Leaf(1), Node(Node(Leaf(2),Leaf(3)),Leaf(4)))"
}

class Example extends FreeSpec {
  "Example" in {
    val prsr = the[Parsable[Tree]].parser()
    assert(prsr.parse(Tree.example).get.value.sum === 10)
  }
}