use str

fn is-sorted [list]{
  n = (- (count $list) 1)
  i = 0

  while (< $i $n) {
    if (> $list[$i] $list[(+ $i 1)]) {
      put $false
      return
    }

    i = (+ $i 1)
  }

  put $true
}

fn bogo-sort [list]{
  while (not (is-sorted $list)) {
    list = [(echo (str:join "\r\n" $list) | shuf)]
  }

  put $list
}

list = [1.0 3.0 2.0 4.0]
bogo-sort $list
