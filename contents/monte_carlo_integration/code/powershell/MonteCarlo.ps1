function Is-InCircle($x, $y, $radius=1) {
    return ([Math]::Pow($x, 2) + [Math]::Pow($y, 2)) -lt [Math]::Pow($radius, 2)
}

function Monte-Carlo([int]$n) {
    $PiCount = 0;
    for ($i = 0; $i -lt $n; $i++) {
        $x = Get-Random -Minimum 0.0 -Maximum 1.0
        $y = Get-Random -Minimum 0.0 -Maximum 1.0

        if (Is-InCircle $x $y) {
            $PiCount++
        }
    }
    return 4.0 * $PiCount / $n
}

# This could take some time
$PiEstimate = Monte-Carlo 10000000
Write-Host "The pi estimate is: $PiEstimate"
Write-Host "Percent error is: $(100 * [Math]::Abs($PiEstimate - ([Math]::PI)) / ([Math]::PI))"