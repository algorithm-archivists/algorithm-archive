import java.util.ArrayList;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Stack;

public class Tree {
    public Node root;

    public Tree(int rowCount, int childrenCount) {
        // this.root is the root node of the Tree
        this.root = new Node(1);
        this.createAllChildren(this.root, rowCount, childrenCount);
    }


    public void dfsRecursive() {
        this.dfsRecursive(this.root);
    }

    private void dfsRecursive(Node node) {
        System.out.println(node.id);

        for (Node n : node.children) {
            dfsRecursive(n);
        }
    }


    public void dfsRecursivePostOrder() {
        this.dfsRecursivePostOrder(this.root);
    }

    private void dfsRecursivePostOrder(Node node) {
        for (Node n : node.children) {
            dfsRecursivePostOrder(n);
        }

        // Here we are doing something ...
        System.out.println(node.id);
    }


    public void dfsRecursiveInOrderBinary() {
        dfsRecursiveInOrderBinary(this.root);
    }

    // This assumes only 2 children
    private void dfsRecursiveInOrderBinary(Node node) {
        switch (node.children.size()) {
            case 2:
                dfsRecursiveInOrderBinary(node.children.get(0));
                System.out.println(node.id);
                dfsRecursiveInOrderBinary(node.children.get(1));
                break;
            case 1:
                dfsRecursiveInOrderBinary(node.children.get(0));
                System.out.println(node.id);
                break;
            case 0:
                System.out.println(node.id);
                break;
            default:
                System.err.println("Not a binary tree at dfsRecursiveInOrderBinary()!");
        }
    }


    public void dfsStack() {
        Stack<Node> stack = new Stack<Node>();
        stack.push(this.root);

        Node tmp;

        while (stack.size() != 0) {
            System.out.println(stack.peek().id);
            tmp = stack.pop();

            for (Node c : tmp.children) {
                stack.push(c);
            }
        }
    }

    public void bfsQueue() {
        Queue<Node> queue = new PriorityQueue<Node>();
        queue.add(this.root);

        while (queue.size() != 0) {
            System.out.println(queue.peek().id);
            Node temp = queue.poll(); // return null if the queue is empty

            if (temp != null) {
                for (Node c : temp.children) {
                    queue.add(c);
                }
            }
        }
    }


    private void createAllChildren(Node node, int rowCount, int childrenCount) {
        if (rowCount <= 1) {
           return; 
        }

        for (int i = 0; i < childrenCount; i++) {
            node.children.add(new Node(node.id * 10 + i + 1));
            createAllChildren(node.children.get(i), rowCount - 1, childrenCount);
        }
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
    
    public static void main(String[] args) {
        System.out.println("Creating Tree");
        Tree tree = new Tree(3, 3);

        System.out.println("Using recursive DFS :");
        tree.dfsRecursive();

        System.out.println("Using stack-based DFS :");
        tree.dfsStack();

        System.out.println("Using queue-based BFS :");
        tree.bfsQueue();

        System.out.println("Using post-order recursive DFS :");
        tree.dfsRecursivePostOrder();


        // Uncommenting the following 2 lines will result in an exception thrown because at least one Node of the Tree has more than 2 children and therefor a DFSRecursiveInorderBinary doesn't work.
        System.out.println("Using in-order binary recursive DFS : (fail)");
        tree.dfsRecursiveInOrderBinary();

        tree = new Tree(3, 2);
        System.out.println("Using in-order binary recursive DFS : (succeed)");
        tree.dfsRecursiveInOrderBinary();


        System.out.println("");
    }
    
}
