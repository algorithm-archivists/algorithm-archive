import scala.collection.mutable._

object TreeTraversal {

  class Tree(val rowCount: Int, val childrenCount: Int) {

    private case class Node(var id: Int) {

      var children = ListBuffer[Node]()
    }

    private val root: Node = Node(1)
    createAllChildren(root, rowCount, childrenCount)

    private def createAllChildren(node: Node, rowCount: Int, childrenCount: Int): Unit = {
      if (rowCount <= 1) return

      0 until childrenCount foreach { i =>
        node.children += Node(node.id * 10 + i + 1)
        createAllChildren(node.children(i), rowCount - 1, childrenCount)
      }
    }

    private def doSomethingWithNode(node: Node) = Console.println(node.id)

    def dfsRecursive(): Unit = {
      def dfsRecursive(node: Node): Unit = {
        doSomethingWithNode(node)
        node.children.foreach(dfsRecursive)
      }

      dfsRecursive(root)
    }

    def dfsRecursivePostOrder(): Unit = {
      def dfsRecursivePostOrder(node: Node): Unit = {
        node.children.foreach(dfsRecursivePostOrder)
        doSomethingWithNode(node)
      }

      dfsRecursivePostOrder(root)
    }

    def dfsRecursiveInOrderBinary(): Unit = {
      def processIfChildExists(children: ListBuffer[Node], index: Int) =
        if (children.isDefinedAt(index))
          dfsRecursiveInOrderBinary(children(index))

      def dfsRecursiveInOrderBinary(node: Node): Unit = {
        if (node.children.size > 2)
          throw new Exception("Not a binary tree!")

        processIfChildExists(node.children, 0)
        doSomethingWithNode(node)
        processIfChildExists(node.children, 1)
      }

      dfsRecursiveInOrderBinary(this.root)
    }

    def dfsStack(): Unit = {
      val stack = new ArrayBuffer[Node]()
      stack += root
      while (stack.nonEmpty) {
        doSomethingWithNode(stack(0))
        stack ++= stack.remove(0).children
      }
    }

    def bfsQueue(): Unit = {
      val queue = new Queue[Node]()
      queue.enqueue(root)
      while (queue.nonEmpty) {
        doSomethingWithNode(queue.head)
        queue ++= queue.dequeue.children
      }
    }

  }

  def main(args: Array[String]): Unit = {
    Console.println("Creating Tree")
    var tree = new Tree(3, 3)

    Console.println("Using recursive DFS :")
    tree.dfsRecursive

    Console.println("Using stack-based DFS :")
    tree.dfsStack

    Console.println("Using queue-based BFS :")
    tree.bfsQueue

    Console.println("Using post-order recursive DFS :")
    tree.dfsRecursivePostOrder

    // Uncommenting the following 2 lines will result in an exception thrown because at least one Node of the Tree
    // has more than 2 children and therefor a DFSRecursiveInorderBinary doesn't work.
    Console.println("Using in-order binary recursive DFS : (fail)")
    tree.dfsRecursiveInOrderBinary

    tree = new Tree(3, 2)
    Console.println("Using in-order binary recursive DFS : (succeed)")
    tree.dfsRecursiveInOrderBinary
  }

}
