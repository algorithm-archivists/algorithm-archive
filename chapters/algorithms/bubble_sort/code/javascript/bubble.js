function bubbleSort(arr) {
  for (let r = arr.length -1; r > 0; r--) {
    for (let i = 0; i < r; i++) {
      if (arr[i] > arr[i + 1]) {
        let tmp = arr[i];
        arr[i] = arr[i + 1];
        arr[i + 1] = tmp;
      }
    }
  }
}
