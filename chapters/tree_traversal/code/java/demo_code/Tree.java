// submitted by xam4lor
import java.util.ArrayList;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Stack;

public class Tree {
    private class Node implements Comparable<Node> {
        public ArrayList<Node> children;
        public int id;

        public Node() {
            this.children = new ArrayList<Node>();
        }

        @Override
        public int compareTo(Node other) {
            // Need to implement Comparable<Node> and override this
            // method because of the method BFSQueue() which uses Queues
            // and must know how to check if two nodes are the same or not
            return Integer.compare(this.id, other.id);
        }
    }


    // Global node
    private Node root;

    public Tree(int depthCount, int childrenCount) {
        this.CreateTree(depthCount, childrenCount);
    }




    public void StartDFSRecursive() {
        this.DFSRecursive(this.root);
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



    private void CreateTree(int depthCount, int childrenCount) {
        this.root = new Node();
        this.root.id = 1;

        this.CreateAllChildren(this.root, depthCount, childrenCount);
    }

    private void CreateAllChildren(Node node, int rowCount, int childrenCount) {
        if(rowCount <= 1)
            return;

        for (int i = 0; i < childrenCount; i++) {
            Node tmp = new Node();
            tmp.id = node.id * 10 + i + 1;

            node.children.add(tmp);

            this.CreateAllChildren(node.children.get(i), rowCount - 1, childrenCount);
        }
    }

    private void DFSRecursive(Node node) {
        System.out.println(node.id);

        for (Node c : node.children) {
            DFSRecursive(c);
        }
    }
}
