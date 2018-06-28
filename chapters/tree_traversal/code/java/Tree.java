// submitted by xam4lor
import java.util.ArrayList;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Stack;

public class Tree {
    // root node of the Tree
    public Node root;

    public Tree(int numberRow, int numberChild) {
        root = createTree(numberRow, numberChild);
    }



	 public void DFSRecursive(Node node) {
	     System.out.println(node.id);

	     for (Node n : node.children) {
	         DFSRecursive(n);
	     }
	 }

	 public void DFSRecursivePostOrder(Node node) {
	     for (Node n : node.children) {
	         DFSRecursivePostOrder(n);
	     }

	     // Here we are doing something ...
	     System.out.println(node.id);
	 }

	 // This assumes only 2 children
	 public void DFSRecursiveInOrderBTree(Node node) {
	     if(node.children.size() > 2) {
	         System.out.println("Not a binary tree!");
	         System.exit(1);
	     }

	     if(node.children.size() > 0) {
	         DFSRecursiveInOrderBTree(node.children.get(0));
	         System.out.println(node.id);
	         DFSRecursiveInOrderBTree(node.children.get(1));
	     }
	     else {
	         System.out.println(node.id);
	     }
	 }

	 public void DFSStack() {
	     Stack<Node> stack = new Stack<Node>();
	     stack.push(this.root);

	     Node tmp;

	     while(stack.size() != 0) {
	         System.out.println(stack.peek().id);
	         tmp = stack.pop();

	         for (Node c : tmp.children) {
	             stack.push(c);
	         }
	     }
	 }

	 public void BFSQueue() {
	     Queue<Node> queue = new PriorityQueue<Node>();
	     queue.add(this.root);

	     Node temp;

	     while(queue.size() != 0) {
	         System.out.println(queue.peek().id);
	         temp = queue.poll(); // return null if the queue is empty

	         if(temp != null) {
	             for (Node c : temp.children) {
	                 queue.add(c);
	             }
	         }
	     }
	 }




     public Node createTree(int numberRow, int numberChild) {
	     Node ret = new Node(numberRow);

	     if(numberRow == 0) {
	         return ret;
	     }

	     for (int i = 1; i < numberChild; i++) {
	         Node child = createTree(numberRow - 1, numberChild);
	         ret.children.add(child);
	     }

		 return ret;
	 }



     private class Node implements Comparable<Node> {
	     public ArrayList<Node> children;
	     public int id;

	     public Node(int id) {
	         this.children = new ArrayList<Node>();
	         this.id = id;
	     }

	     @Override
	     public int compareTo(Node other) {
	         // Need to implement Comparable<Node> and override this
	         // method because of the method BFSQueue() which uses Queues
	         // and must know how to check if two nodes are the same or not
	         return Integer.compare(this.id, other.id);
	     }
	 }
}
