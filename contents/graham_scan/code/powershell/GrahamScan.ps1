function New-Point($x, $y) {
    return New-Object -TypeName PSObject -Property @{X = [int]$x; Y = [int]$y}
}

function CounterClockWise($p1, $p2, $p3) {
    return (($p3.Y - $p1.Y) * ($p2.X - $p1.X)) -ge (($p2.Y - $p1.Y) * ($p3.X - $p1.X))
}

function Graham-Scan([System.Collections.ArrayList] $gift) {
    [System.Collections.ArrayList]$gift = $gift | Get-Unique -AsString
    $start = $gift | Sort-Object -Property Y | Select-Object -First 1
    $gift.Remove($start)

    $s = $gift | Sort-Object -Property {[Math]::Atan2($_.Y - $start.Y, $_.X - $start.X)}
    [System.Collections.ArrayList]$hull = @($start, $s[0], $s[1])

    $s | Select-Object -Skip 2 | ForEach-Object {
        while (-not (CounterClockWise $hull[-2] $hull[-1] $_)) {
            $hull.RemoveAt($hull.Count - 1)
        }
        $hull.Add($_) | Out-Null
    }

    return $hull
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

$hull = Graham-Scan $testGift

Write-Host "The points in the hull are: $($hull | % {"($($_.X), $($_.Y)) "})"