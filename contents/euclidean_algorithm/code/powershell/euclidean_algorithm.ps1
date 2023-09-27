function Sub-Euclid($a, $b) {
    $a = [Math]::Abs($a)
    $b = [Math]::Abs($b)

    while ($a -ne $b) {
        if ($a -gt $b) {
            $a = $a - $b
        } else {
            $b = $b - $a
        }
    }
    
    return $a
}

function Mod-Euclid($a, $b) {
    $a = [Math]::Abs($a)
    $b = [Math]::Abs($b)

    while ($b -ne 0) {
        $tmp = $b
        $b = $a % $b
        $a = $tmp
    }

    return $a
}

Write-Host "[#]`nSubtraction-based euclidean algorithm result:`n$(Mod-Euclid $(64 * 67) $(64 * 81))"
Write-Host "[#]`nModulus-based euclidean algorithm result:`n$(Sub-Euclid $(128 * 12) $(128 * 77))"
