
import java.util.*;

class TreeNode {
    String key = "";
    int value = 0;
    TreeNode left = null, right = null;

    public TreeNode(String key, int value) {
        this.key = key;
        this.value = value;
    }

    public TreeNode(int value, TreeNode left, TreeNode right) {
        this.value = value;
        this.left = left;
        this.right = right;
    }

}

class HuffmanTree {
    private Map<String, Integer> map = new HashMap<>();
    private Map<String, String> codeBook = new HashMap<>(), reverseCodeBook = new HashMap<>();
    private TreeNode root;
    private String s;

    public HuffmanTree(String s) {
        this.s = s;
    }

    public void createTree() {
        for (int i = 0; i < s.length(); i++) {
            String key = Character.toString(s.charAt(i));

            if (!map.containsKey(key)) map.put(key, 1);
            else {
                int frequency = map.get(key) + 1;
                map.replace(key, frequency);
            }
            
        }

        Comparator<TreeNode> nodeComparator = Comparator.comparingInt(o -> o.value);

        Queue<TreeNode> priorityQueue = new PriorityQueue<>(nodeComparator);
        for (Map.Entry<String, Integer> m : map.entrySet()) {
            priorityQueue.add(new TreeNode(m.getKey(), m.getValue()));
        }


        while (priorityQueue.size() > 1) {
            TreeNode temp1 = priorityQueue.remove();
            TreeNode temp2 = priorityQueue.remove();
            TreeNode node;
            node = new TreeNode(temp1.value + temp2.value, temp1, temp2);
            priorityQueue.add(node);
        }
        root = priorityQueue.remove();

    }

    private void traverse(TreeNode temp, String w) {
        if (temp.left == null && temp.right == null)
            codeBook.put(temp.key, w);
        if (temp.left != null) traverse(temp.left, w + 0);
        if (temp.right != null) traverse(temp.right, w + 1);

    }

    public void printCodeBook() {
        System.out.println("Code Book");
        for (Map.Entry<String, String> m : codeBook.entrySet()) {
            System.out.println(m.getKey() + "\t" + m.getValue());
        }
        System.out.println();
    }

    private void CodeBookReverse() {
        for (Map.Entry<String, String> m : codeBook.entrySet()) {
            reverseCodeBook.put(m.getValue(), m.getKey());
        }
    }

    public String encode() {
        traverse(root, "");
        String enc = "";
        for (int i = 0; i < s.length(); i++) {
            String k = Character.toString(s.charAt(i));
            enc += codeBook.get(k);
        }
        printCodeBook();
        return enc;
    }

    public String decode(String enc) {
        String dec = "", key = "";
        CodeBookReverse();

        for (int i = 0; i < enc.length(); i++) {
            key = key + enc.charAt(i);
            if (reverseCodeBook.containsKey(key)) {
                dec = dec + reverseCodeBook.get(key);
                key = "";
            }
        }
        return dec;
    }


}

class Huffman {

    public static void main(String[] args) {
        String s = "bibbity_bobbity";
        HuffmanTree huffmanTree = new HuffmanTree(s);
        huffmanTree.createTree();
        String encoded = huffmanTree.encode();
        System.out.println("Encoded String: " + encoded);
        System.out.println("Decoded String: " + huffmanTree.decode(encoded));


    }
}
