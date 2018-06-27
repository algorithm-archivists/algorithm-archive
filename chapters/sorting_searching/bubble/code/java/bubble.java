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
