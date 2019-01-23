public class Bubble {
    static void bubbleSort(int[] arr) {
        for (int r = arr.length - 1; r > 0; r--) {
            for (int i = 0; i < r; i++) {
                if(arr[i] > arr[i + 1]) {
                    int tmp = arr[i];
                    arr[i] = arr[i + 1];
                    arr[i + 1] = tmp;
                }
            }
        }
    }

    public static void main(String[] args) {
        int[] test = new int[]{20, -3, 50, 1, -6, 59};

        System.out.println("Unsorted array :");
        for (int i = 0; i < test.length; i++) {
            System.out.print(test[i] + " ");
        }

        bubbleSort(test);

        System.out.println("\n\nSorted array :");
        for (int i = 0; i < test.length; i++) {
            System.out.print(test[i] + " ");
        }
        System.out.println("");
    }
}
