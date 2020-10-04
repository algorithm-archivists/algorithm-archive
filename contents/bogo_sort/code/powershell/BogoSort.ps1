function Is-Sorted($list) {
    for ($i = 0; $i -lt $list.Count - 1; $i++) {
        if ($list[$i] -gt $list[$i + 1]) {
            return $false
        }
    }
    return $true
}

function Sort-Bogo($list) {
    while (-not (Is-Sorted $list)) {
        # shuffle the list
        $list = $list | Get-Random -Count $list.Count
    }
    return $list
}

$list = @(1, 3654, 78, 654, -234, -12, 4, 3, -6, -100)
Write-Host "Unsorted list:`n$list"
Write-Host "`n"

# this might take a while
$list = Sort-Bogo $list

Write-Host "Sorted list:`n$list"
