import scala.collection.mutable.{ListBuffer, Map, PriorityQueue}

object HuffmanEncoding {

  trait Node {
    var weight: Int
  }

  case class Leaf(char: Char, var weight: Int) extends Node

  case class Branch(left: Node, right: Node, var weight: Int) extends Node {}

  def createTree(phrase: String) = {
    val leaves = ListBuffer[Leaf]()
    phrase.foreach(c =>
      leaves.find(leaf => leaf.char == c) match {
        case Some(leaf) => leaf.weight += 1
        case _ => leaves += Leaf(c, 1)
      }
    )

    val tree = PriorityQueue[Node]()(Ordering.by(-_.weight))
    tree ++= leaves
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
    val encoded: StringBuilder = StringBuilder.newBuilder
    phrase.foreach(c =>
      encoded ++= codeBook(c)
    )
    encoded.mkString
  }

  def decode(encoded: String, root: Node) = {
    val decoded: StringBuilder = StringBuilder.newBuilder
    var currentNode = root
    encoded.foreach(bit => {
      currentNode match {
        case Branch(left, right, _) =>
          currentNode = if (bit == '0') left else right
      }
      currentNode match {
        case Leaf(c, _) => {
          decoded += c
          currentNode = root
        }
        case _ =>
      }
    })
    decoded.mkString
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
