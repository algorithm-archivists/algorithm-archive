function mergeSort(array) {
  if (array.length <= 1) {
    return array;
  }

  const middle = Math.floor(array.length / 2);
  const leftHalf = array.slice(0, middle);
  const rightHalf = array.slice(middle);

  return merge(mergeSort(leftHalf), mergeSort(rightHalf));
}

function merge(left, right) {
  const sorted = [];
  let indexLeft = 0;
  let indexRight = 0;

  while (indexLeft < left.length && indexRight < right.length) {
    if (left[indexLeft] < right[indexRight]) {
      sorted.push(left[indexLeft]);
      indexLeft++;
    } else {
      sorted.push(right[indexRight]);
      indexRight++;
    }
  }

  return sorted.concat(left.slice(indexLeft), right.slice(indexRight));
}

const unsortedText = ['M','E','R','G','E','S','O','R','T','E','X','A','M','P','L','E'];
const sortedText = mergeSort(unsortedText);

console.log(unsortedText);
console.log(sortedText);

// input:  ['M', 'E', 'R', 'G', 'E', 'S', 'O', 'R', 'T', 'E', 'X', 'A', 'M', 'P', 'L', 'E']
// result: ["A", "E", "E", "E", "E", "G", "L", "M", "M", "O", "P", "R", "R", "S", "T", "X"]
