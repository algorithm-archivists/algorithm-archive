function New-Point($x, $y) {
    return New-Object -TypeName PSObject -Property @{X = [int]$x; Y = [int]$y}
}

function CounterClockWise($p1, $p2, $p3) {
    return (($p3.Y - $p1.Y) * ($p2.X - $p1.X)) -ge (($p2.Y - $p1.Y) * ($p3.X - $p1.X))
}

function jarvis-march($gift) {
    [System.Collections.ArrayList]$gift = $gift | Get-Unique -AsString
    $pointOnHull = $gift | Sort-Object -Property X | Select-Object -First 1
    [System.Collections.ArrayList]$hull = @($pointOnHull)

    while ($true) {
        $endpoint = $gift[0]
        
        $gift | Select-Object -Skip 1 | ForEach-Object {
            if (($endpoint -eq $pointOnHull) -or (-not (CounterClockWise $_ $hull[-1] $endpoint))) {
                $endpoint = $_
            }
        }

        $pointOnHull = $endpoint

        if ($hull[0] -eq $endpoint) {
            return $hull
        } else {
            $hull.Add($pointOnHull) | Out-Null
        }
    }
}

$testGift = @(
        (New-Point -5 2)
        (New-Point 5 7)
        (New-Point -6 -12)
        (New-Point -14 -14)
        (New-Point 9 9)
        (New-Point -1 -1)
        (New-Point -10 11)
        (New-Point -6 15)
        (New-Point -6 -8)
        (New-Point 15 -9)
        (New-Point 7 -7)
        (New-Point -2 -9)
        (New-Point 6 -5)
        (New-Point 0 14)
        (New-Point 2 8)
)

$hull = Jarvis-March $testGift
Write-Host "The points in the hull are: $($hull | % {"($($_.X), $($_.Y)) "})"