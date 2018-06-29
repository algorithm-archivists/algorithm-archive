// submitted by xam4lor
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
        if (node.children.size() > 2) {
            System.err.println("Not a binary tree at dfsRecursiveInOrderBinary()!");
            return;
        }

        if (node.children.size() > 1) {
            dfsRecursiveInOrderBinary(node.children.get(0));
            System.out.println(node.id);
            dfsRecursiveInOrderBinary(node.children.get(1));
        } else {
            System.out.println(node.id);
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
}
