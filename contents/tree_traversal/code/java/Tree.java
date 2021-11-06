import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Stack;

public class Tree {
    public Node root;

    public Tree(int rowCount, int childrenCount) {
        // this.root is the root node of the Tree
        this.root = new Node(rowCount);
        this.createAllChildren(this.root, rowCount-1, childrenCount);
    }


    public void dfsRecursive() {
        this.dfsRecursive(this.root);
    }

    private void dfsRecursive(Node node) {
        System.out.print(node.id + " ");

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
        System.out.print(node.id + " ");
    }


    public void dfsRecursiveInOrderBinary() {
        dfsRecursiveInOrderBinary(this.root);
    }

    private void dfsRecursiveInOrderBinary(Node node) {
        switch (node.children.size()) {
            case 2:
                dfsRecursiveInOrderBinary(node.children.get(0));
                System.out.print(node.id + " ");
                dfsRecursiveInOrderBinary(node.children.get(1));
                break;
            case 1:
                dfsRecursiveInOrderBinary(node.children.get(0));
                System.out.print(node.id + " ");
                break;
            case 0:
                System.out.print(node.id + " ");
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
            System.out.print(stack.peek().id + " ");
            tmp = stack.pop();

            for (Node c : tmp.children) {
                stack.push(c);
            }
        }
    }

    public void bfsQueue() {
        Queue<Node> queue = new LinkedList<Node>();
        queue.add(this.root);

        while (queue.size() != 0) {
            System.out.print(queue.peek().id + " ");
            Node temp = queue.poll(); // return null if the queue is empty

            if (temp != null) {
                for (Node c : temp.children) {
                    queue.add(c);
                }
            }
        }
    }


    private void createAllChildren(Node node, int rowCount, int childrenCount) {
        if (rowCount < 0) {
           return; 
        }

        for (int i = 0; i < childrenCount; i++) {
            node.children.add(new Node(rowCount));
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
        Tree tree = new Tree(2, 3);

        System.out.println("[#]\nRecursive DFS:");
        tree.dfsRecursive();
        System.out.println();

        System.out.println("[#]\nRecursive Postorder DFS:");
        tree.dfsRecursivePostOrder();
        System.out.println();


        System.out.println("[#]\nStack-based DFS:");
        tree.dfsStack();
        System.out.println();


        System.out.println("[#]\nQueue-based BFS:");
        tree.bfsQueue();
        System.out.println();


        // Uncommenting the following 2 lines will result in an exception thrown because at least one Node of the Tree has more than 2 children and therefor a DFSRecursiveInorderBinary doesn't work.
        //System.out.println("Using in-order binary recursive DFS : (fail)");
        //tree.dfsRecursiveInOrderBinary();

        tree = new Tree(3, 2);
        System.out.println("[#]\nRecursive Inorder DFS for Binary Tree:");
        tree.dfsRecursiveInOrderBinary();
        System.out.println();
    }
}
