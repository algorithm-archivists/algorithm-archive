public class Bogo {
    static void bogoSort(int[] arr) {
        while(!isSorted(arr)) {
            shuffle(arr);
        }
    }

    static boolean isSorted(int[] arr) {
        for (int i = 0; i < arr.length - 1; i++) {
            if(arr[i] > arr[i + 1]) {
                return false;
            }
        }

        return true;
    }

    static void shuffle(int[] arr) {
        for (int r = arr.length - 1; r > 0; r--) {
            int i = (int) Math.floor(Math.random() * r);
            int tmp = arr[i];
            arr[i] = arr[r];
            arr[r] = tmp;
        }
    }


    public static void main(String[] args) {
        int[] test = new int[]{20, -3, 50, 1, -6, 59};

        System.out.println("Unsorted array :");
        for (int i = 0; i < test.length; i++) {
            System.out.print(test[i] + " ");
        }


        bogoSort(test);


        System.out.println("\n\nSorted array :");
        for (int i = 0; i < test.length; i++) {
            System.out.print(test[i] + " ");
        }
	System.out.println("");
    }
}
