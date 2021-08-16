import java.util.*;

class TreeNode {
    String letter = "";
    int frequency = 0;
    TreeNode left = null, right = null;

    public TreeNode(String letter, int frequency) {
        this.letter = letter;
        this.frequency = frequency;
    }

    public TreeNode(int frequency, TreeNode left, TreeNode right) {
        this.frequency = frequency;
        this.left = left;
        this.right = right;
    }
}

class HuffmanTree {
    private Map<String, Integer> frequencyMap = new HashMap<>();
    private Map<String, String> codeBook = new HashMap<>(), reverseCodeBook = new HashMap<>();
    private TreeNode root;
    private String stringToEncode;

    public HuffmanTree(String stringToEncode) {
        this.stringToEncode = stringToEncode;
    }

    public void createTree() {
        for (int i = 0; i < stringToEncode.length(); i++) {
            String key = Character.toString(stringToEncode.charAt(i));
            if (!frequencyMap.containsKey(key)) {
                frequencyMap.put(key, 1);
            } else {
                int frequency = frequencyMap.get(key) + 1;
                frequencyMap.replace(key, frequency);
            }
        }
        Queue<TreeNode> priorityQueue = new PriorityQueue<>(Comparator.comparingInt(o -> o.frequency));
        for (Map.Entry<String, Integer> m : frequencyMap.entrySet()) {
            priorityQueue.add(new TreeNode(m.getKey(), m.getValue()));
        }
        while (priorityQueue.size() > 1) {
            TreeNode left = priorityQueue.remove();
            TreeNode right = priorityQueue.remove();
            priorityQueue.add(new TreeNode(left.frequency + right.frequency, left, right));
        }
        root = priorityQueue.remove();
    }

    private void traverse(TreeNode node, StringBuilder code) {
        if (node.left == null && node.right == null) {
            codeBook.put(node.letter, code.toString());
        }
        if (node.left != null) {
            traverse(node.left, code.append(0));
            code.deleteCharAt(code.length() - 1);
        }
        if (node.right != null) {
            traverse(node.right, code.append(1));
            code.deleteCharAt(code.length() - 1);
        }
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
        traverse(root, new StringBuilder());
        StringBuilder encode = new StringBuilder();
        for (int i = 0; i < stringToEncode.length(); i++) {
            String k = Character.toString(stringToEncode.charAt(i));
            encode.append(codeBook.get(k));
        }
        printCodeBook();
        return encode.toString();
    }

    public String decode(String encoded) {
        StringBuilder decoded = new StringBuilder(), key = new StringBuilder();
        CodeBookReverse();
        for (int i = 0; i < encoded.length(); i++) {
            key = key.append(encoded.charAt(i));
            if (reverseCodeBook.containsKey(key.toString())) {
                decoded.append(reverseCodeBook.get(key.toString()));
                key = new StringBuilder();
            }
        }
        return decoded.toString();
    }
}

class Huffman {
    public static void main(String[] args) {
        HuffmanTree huffmanTree = new HuffmanTree("bibbity_bobbity");
        huffmanTree.createTree();
        String encoded = huffmanTree.encode();
        System.out.println("Encoded String: " + encoded);
        System.out.println("Decoded String: " + huffmanTree.decode(encoded));
    }
}
