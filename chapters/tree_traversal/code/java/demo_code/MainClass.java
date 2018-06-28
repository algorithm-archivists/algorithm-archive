//submitted by xam4lor
public class MainClass {
    public static void main(String[] args) {
        System.out.println("Tree Traversal");
        Tree tree = new Tree(3, 3);

        System.out.println("StartDFSRecursive");
        tree.StartDFSRecursive();

        System.out.println("DFSStack");
        tree.DFSStack();

        System.out.println("BFSQueue");
        tree.BFSQueue();

        System.out.println("");
    }
}
