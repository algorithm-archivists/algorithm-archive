function isSorted(arr) {
  for (let i = 0; i < arr.length - 1; i++) {
    if (arr[i] > arr[i + 1]) {
      return false;
    }
  }

  return true;
}

// The shuffle() function can be found in code/js/bogo.js
function bogoSort(arr) {
  while (!isSorted(arr)) {
    shuffle(arr);
  }
}

function shuffle(arr) {
  for (let r = arr.length -1; r > 0; r--) {
    let i = Math.floor(Math.random() * r);
    let tmp = arr[i];
    arr[i] = arr[r];
    arr[r] = tmp;
  }
}
