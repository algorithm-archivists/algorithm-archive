public class Node {
    public ArrayList<Node> children = new ArrayList<Node>();
    public int id;
}


public void DFSRecursive(Node node) {
    // Here we are doing something
    System.out.println(node.id);

    for (Node c : node.children) {
        DFSRecursive(c);
    }
}


public void DFSRecursivePostorder(Node node) {
    for (Node c : node.children) {
        DFSRecursivePostorder(c);
    }

    // Here we are doing something...
    System.out.println(node.id);
}


// This assumes only 2 children
public void DFSRecursiveInorderBinary(Node node) {
    if (node.children.size() > 2) {
        System.err.println(node.id);
    }

    if (node.children.size() > 0) {
        DFSRecursiveInorderBinary(node.children.get(0));
        System.out.println(node.id);
        DFSRecursiveInorderBinary(node.children.get(1));
    }
    else {
        System.out.println(node.id);
    }
}


public void DFSStack(Node node) {
    Stack<Node> stack = new Stack<Node>();
    stack.push(node);

    Node tmp;

    while(stack.size() != 0) {
        System.out.println(stack.peek().id);
        tmp = stack.pop();

        for (Node c : tmp.children) {
            stack.push(c);
        }
    }
}


public void BFSQueue(Node node) {
    Queue<Node> queue = new PriorityQueue<Node>();
    queue.add(node);
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
