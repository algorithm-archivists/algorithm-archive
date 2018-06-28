//submitted by xam4lor
public class MainClass {
	public static void main(String[] args) {
	     System.out.println("Creating Tree");
	     Tree tree = new Tree(2, 3);

	     System.out.println("Using recursive DFS :");
	     tree.DFSRecursive(tree.root);

	     System.out.println("Using stack-based DFS :");
	     tree.DFSStack();

	     System.out.println("Using queue-based BFS :");
	     tree.BFSQueue();

	     System.out.println("");
	 }
}
