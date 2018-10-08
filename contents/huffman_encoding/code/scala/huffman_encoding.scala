import scala.collection.mutable.{Map, PriorityQueue}

object HuffmanEncoding {

  trait Node {
    var weight: Int
  }

  case class Leaf(char: Char, var weight: Int) extends Node

  case class Branch(left: Node, right: Node, var weight: Int) extends Node

  def createTree(phrase: String) = {

    val tree = PriorityQueue[Node]()(Ordering.by(-_.weight))
    tree ++= phrase
      .groupBy(identity)
      .mapValues(_.size)
      .map{
        case (char, count) => Leaf(char, count)
      }

    while (tree.size > 1) {
      val node1 = tree.dequeue()
      val node2 = tree.dequeue()
      tree += Branch(node1, node2, node1.weight + node2.weight)
    }

    tree.head
  }


  def createCodeBook(root: Node) = {
    val codeBook = Map[Char, String]()

    def codeBookRecurse(node: Node, code: String): Unit =
      node match {
        case Leaf(symbol, _) => codeBook.put(symbol, code)
        case Branch(left, right, _) => {
          codeBookRecurse(left, code + "0")
          codeBookRecurse(right, code + "1")
        }
      }

    codeBookRecurse(root, "")
    codeBook
  }


  def encode(phrase: String, codeBook: Map[Char, String]) = {
    phrase.flatMap(c => codeBook(c))
  }

  def decode(encoded: String, root: Node) = {
    var currentNode = root

    def chooseTreeBranch(bit: Char) =
      currentNode match {
        case Branch(left, right, _) =>
          currentNode = if (bit == '0') left else right
      }

    def maybeGetACharacter =
      currentNode match {
        case Leaf(c, _) => {
          currentNode = root
          Some(c)
        }
        case _ => None
      }

    encoded
      .flatMap(bit => {
        chooseTreeBranch(bit)
        maybeGetACharacter
      })
  }

  def main(args: Array[String]): Unit = {
    val originalText = "bibbity_bobbity"
    println("Original Text: " + originalText)

    val tree = createTree("bibbity_bobbity")
    val codeBook = createCodeBook(tree)
    println("CodeBook is: " + codeBook)

    val encoded = encode("bibbity_bobbity", codeBook)
    println("Encoded text: " + encoded)

    val decoded = decode(encoded, tree)
    println("Decoded text: " + decoded)

  }

}
