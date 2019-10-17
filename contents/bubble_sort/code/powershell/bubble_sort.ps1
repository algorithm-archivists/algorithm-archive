function Sort-Bubble($list) {
    for ($i = 0; $i -lt $list.Count; $i++) {
        for ($j = 0; $j -lt $list.Count - 1; $j++) {
            if ($list[$j] -gt $list[$j + 1]) {
                $tmp = $list[$j]
                $list[$j] = $list[$j + 1]
                $list[$j + 1] = $tmp
            }
        }
    }
}


$test = @(20, -3, 50, 1, -6, 59)

Write-Host "unsorted list:"
Write-Host $test -Separator ", "

Sort-Bubble $test

Write-Host "sorted list:"
Write-Host $test -Separator ", "
