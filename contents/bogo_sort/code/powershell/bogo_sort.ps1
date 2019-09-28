function Is-Sorted($list) {
    for($i = 0; $i -lt $list.Count - 1; $i++) {
        if ($list[$i] -gt $list[$i + 1]) {
            return $false
        }
    }
    return $true
}

function Shuffle-List($list) {
    for($i = 0; $i -lt $list.Count; $i++) {
        $random = Get-Random -Minimum 0 -Maximum $list.Count

        $temp = $list[$i]
        $list[$i] = $list[$random]
        $list[$random] = $temp
    }
}

function Sort-Bogo($list) {
   while (-not $(Is-Sorted $list)) {
       Shuffle-List $list
   }
}


$test = @(20, -3, 50, 1, -6, 59)

Write-Host "unsorted list:"
Write-Host $test -Separator ", "

Sort-Bogo $test

Write-Host "sorted list:"
Write-Host $test -Separator ", "
