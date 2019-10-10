import java.util.*

// node type
sealed class Node(val frequency: Int): Comparable<Node> {
  // we can sort nodes by frequency
  override fun compareTo(other: Node) = frequency - other.frequency
}
class Leaf(freq: Int, val letter: Char): Node(freq)
class Branch(freq: Int, val left: Node, val right: Node): Node(freq)

class HuffmanTree (val textSample: String) {
  val root: Node
  val codeBook: CodeBook
  
  init {
    // calculate frequencies
    val frequencyMap = HashMap<Char, Int>()
    for (char in textSample) {
      val newFrequency = (frequencyMap.get(char) ?: 0) + 1
      frequencyMap.put(char, newFrequency)
    }
    // populate the queue with leaves
    val priorityQueue = PriorityQueue<Node>()
    for (m in frequencyMap.entries) {
      priorityQueue.add(Leaf(m.value, m.key))
    }
    // join leaves
    while (priorityQueue.size > 1) {
      val left = priorityQueue.remove()
      val right = priorityQueue.remove()
      priorityQueue.add(Branch(left.frequency + right.frequency, left, right))
    }
    // the root node is the last remaining leaf
    root = priorityQueue.remove()
    // initialize the code book
    codeBook = CodeBook(root)
  }
  
  // encode a string using the tree
  fun encode(source: String): String {
    val encoder = StringBuilder()
    for (char in source) {
      encoder.append(codeBook.encode(char))
    }
    return encoder.toString()
  }
  
  // decode an encoded string
  fun decode(encoded: String) = buildString {
    var currentSequence = StringBuilder()
    for (k in encoded) {
      currentSequence.append(k)
      val decoded = codeBook.decode(currentSequence.toString())
      if(decoded != null) {
        append(decoded)
        currentSequence = StringBuilder()
      }
    }
  }
}

// a codebook is a mapping between a huffman code and a character, and the other way around
class CodeBook(tree: Node) {
  val codebook: Map<Char, String>
        
  init {
    codebook = HashMap<Char, String>()
    populateCodebook(tree, "", codebook)
  }
  
  // recursively populate a codebook with encodings from a node
  fun populateCodebook(node: Node, code: String, codebook: MutableMap<Char, String>) {
    when(node) {
      // leaf node, add current children
      is Leaf -> codebook.put(node.letter, code.toString())
      is Branch -> {
        // populate using left and right children
        populateCodebook(node.left, code + "0", codebook)
        populateCodebook(node.right, code + "1", codebook)
      }
    }
  }
  
  // encode a letter
  fun encode(letter: Char) = codebook[letter]
  
  // the reverse codebook is just the original one with keys as values and values as keys
  val reverseCodebook: HashMap<String, Char> by lazy {
    val reversed = HashMap<String, Char>()
    for (m in codebook.entries) {
      reversed.put(m.value, m.key)
    }
    reversed
  }
  // decode a sequence of bits by looking them up in the reverse codebook
  fun decode(encoded: String) = reverseCodebook[encoded]
  
  // print the codebook to stdout
  fun print() {
    for (m in codebook.entries) {
      println(m.key + ": " + m.value)
    }
    println()
  }
  
  // print the reverse codebook to stdout
  fun printReverse() {
    for (m in reverseCodebook.entries) {
      println(m.key + ": " + m.value)
    }
    println()
  }
}

fun main(args: Array<String>) {
  val sourceText = "bibbity_bobbity"
  // create huffman tree
  val huffmanTree = HuffmanTree(sourceText)
  // encode the text
  val encoded = huffmanTree.encode(sourceText)
  println("Encoded String: " + encoded)
  println("Decoded String: " + huffmanTree.decode(encoded))
  // create a separate codebook and print it
  val codeBook = CodeBook(huffmanTree.root)
  println("Codebook:")
  codeBook.print()
  println("Reverse codebook:")
  codeBook.printReverse()
}

