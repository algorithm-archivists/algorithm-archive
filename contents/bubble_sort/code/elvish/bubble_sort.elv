fn bubble-sort [list]{
  last-index = (- (count $list) 1)

  for _ $list {
    i = 0
    while (< $i $last-index) {
      if (> $list[$i] $list[(+ $i 1)]) {
        tmp = $list[$i]
        list[$i] = $list[(+ $i 1)]
        list[(+ $i 1)] = $tmp
      }

      i = (+ $i 1)
    }
  }

  put $list
}

list = [1. 3 2 4 5 10 50 7 1.5 0.3]
echo Unsorted: $list
echo Sorted: (bubble-sort $list)
