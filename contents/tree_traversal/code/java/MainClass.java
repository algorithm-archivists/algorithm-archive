//submitted by xam4lor
public class MainClass {
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
