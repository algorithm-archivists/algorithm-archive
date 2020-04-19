bubbleSort = (a) ->
  n = a.length
  for i in [0 .. n]
    for j in [0 .. n - 1]
      if a[j] > a[j + 1]
        [a[j + 1], a[j]] = [a[j], a[j + 1]]

main = () ->
  a = [1, 3, 2, 4, 5, 10, 50, 7, 1.5, 0.3]
  bubble_sort(a)
  console.log(a)

main()
