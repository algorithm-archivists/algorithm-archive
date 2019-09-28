function Solve-Euler($timestep, $n) {
    $eulerResult = @(1)

    For ($i = 1; $i -lt $n; $i++) {
        $eulerResult += $eulerResult[$i - 1] - (3 * $eulerResult[$i - 1] * $timestep)
    }

    return $eulerResult
}

function Check-Result($eulerResult, $threshold, $timestep) {
    $isApprox = $true

    For ($i = 0; $i -lt $eulerResult.Count; $i++) {
        $time = $i * $timestep
        $solution = [Math]::Exp(-3 * $time)

        if ([Math]::Abs($eulerResult[$i] - $solution) -gt $threshold) {
            Write-Host $eulerResult[$i] $solution
            $isApprox = $false 
        }
    }

    return $isApprox
}


$timestep = 0.1
$n = 100
$threshold = 0.1

$eulerResult = Solve-Euler $timestep $n
$isApprox = Check-Result $eulerResult $threshold $timestep

Write-Host $isApprox
