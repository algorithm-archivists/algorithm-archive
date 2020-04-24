function bubbleSort(arr) {
  let tmp;
  for (let i = 0; i < arr.length; i++) {
    for (let k = 0; k < arr.length - 1; k++) {
      if (arr[k] > arr[k + 1]) {
        tmp = arr[k];
        arr[k] = arr[k + 1];
        arr[k + 1] = tmp;
      }
    }
  }
}

function main() {
  const testArray = [1, 3, 2, 4, 5, 10, 50, 7, 1.5, 0.3];
  bubbleSort(testArray);
  console.log(testArray);
}

main();
