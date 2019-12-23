import java.util.PriorityQueue

// Node type
sealed class Node(val frequency: Int): Comparable<Node> {
    // So can sort nodes by frequency
    override fun compareTo(other: Node) = frequency - other.frequency
}

class Leaf(freq: Int, val letter: Char): Node(freq)
class Branch(val left: Node, val right: Node): Node(left.frequency + right.frequency)


// Class to contain the decoder and encoder
class HuffmanTree (textSample: String) {
    private val root: Branch
    private val codeBook: MutableMap<Char, String>

    init {
        // Calculate frequencies
        val frequencyMap = textSample.groupingBy { it }.eachCount()

        // Populate the queue with leaves
        val priorityQueue = PriorityQueue<Node>(frequencyMap.map{(char,freq)->
            Leaf(freq, char)
        })

        // Join leaves
        while (priorityQueue.size > 1) {
            val left = priorityQueue.remove()
            val right = priorityQueue.remove()
            priorityQueue.add(Branch(left, right))
        }

        // The root node is the last remaining branch. If it's a leaf, we only had 1 unique letter
        root = priorityQueue.remove() as? Branch ?: error("No support for string of one unique letter")

        // Initialize the code book
        codeBook = mutableMapOf()
        populateCodeBook(root,"")
    }

    // Recursively populate a codebook with encodings from a node
    private fun populateCodeBook(node: Node, code: String) {
        when(node) {
            // Leaf node, set this char's code
            is Leaf -> codeBook[node.letter] = code

            // Branch node, recursively populate the children
            is Branch -> {
                populateCodeBook(node.left, code + "0")
                populateCodeBook(node.right, code + "1")
            }
        }
    }

    // Print the code book to stdout
    fun print() {
        for ((k,v) in codeBook.entries) {
            println("$k: $v")
        }
        println()
    }

    // Encode a string using this tree
    fun encode(source: String): String {
        val encoder = StringBuilder()
        for (char in source) {
            encoder.append(codeBook[char])
        }
        return encoder.toString()
    }


    // Decode an encoded string
    fun decode(encoded: String) = buildString {
        val endNode = encoded.fold(root){current, char->
            val next = when(char){
                '0' -> current.left
                '1' -> current.right
                else -> error("Found `$char` in the encoded string")
            }
            when(next){
                is Branch -> next
                is Leaf -> {
                    append(next.letter)
                    root
                }
            }
        }

        if(endNode != root)
            error("Unexpected end of string")
    }
}


fun main() {
    val sourceText = "bibbity_bobbity"
    // Create huffman tree
    val huffmanTree = HuffmanTree(sourceText)
    // Encode the text
    val encoded = huffmanTree.encode(sourceText)
    println("Encoded String: $encoded")
    println("Decoded String: ${huffmanTree.decode(encoded)}")

    println("CodeBook:")
    huffmanTree.print()
}
