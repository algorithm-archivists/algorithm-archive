void main() {
  List<int> list = [5, 1, 4, 2, 8];
  List<int> sortedarray = bubbleSort(list);
  print(sortedarray);
}



bubbleSort(List<int> list) {
  var sorted = false;
  var counter = 0;

  while (!sorted) {
    sorted = true;
    for (var i = 0; i < list.length - 1 - counter; i++) {
      if (list[i] > list[i + 1]) {
        final tmp = list[i];
        list[i] = list[i + 1];
        list[i + 1] = tmp;
        sorted = false;
      }
    }
    counter++;
  }
  return list;
}

