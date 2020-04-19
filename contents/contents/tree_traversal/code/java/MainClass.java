//submitted by xam4lor
public class MainClass {
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
