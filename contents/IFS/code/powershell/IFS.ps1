# This function simulates a "chaos game"
function Simulate-ChaosGame($n, $shapePoints) {
    $outputPoints = New-Object System.Collections.ArrayList
    
    # Initialize the starting point
    $point = @($(Get-Random -Minimum 0.0 -Maximum 1.0), $(Get-Random -Minimum 0.0 -Maximum 1.0))

    for ($i = 0; $i -lt $n; $i++) {
      $outputPoints.add($point) | Out-Null
      $temp = $shapePoints[$(Get-Random -Maximum $shapePoints.Count)]

      $point = @(
          0.5 * ($point[0] + $temp[0])
          0.5 * ($point[1] + $temp[1])
      )
    }

    return $outputPoints
}


# This will generate a Sierpinski triangle with a chaos game of n points for an
# initial triangle with three points on the vertices of an equilateral triangle:
#     A = (0.0, 0.0)
#     B = (0.5, sqrt(0.75))
#     C = (1.0, 0.0)
# It will output the file sierpinski.dat, which can be plotted after
$shapePoints = @(
    @(0.0, 0.0),
    @(0.5, [math]::sqrt(0.75)),
    @(1.0, 0.0)
)

Simulate-ChaosGame -n 10000 -shapePoints $shapePoints | % { "$($_[0])`t$($_[1])" } | Out-File -Path "sierpinski.dat"
