function Calculate-Computus([int]$Year, [switch]$Servois) {

    # Year's position on the 19 year metonic cycle
    $a = $Year % 19

    # Century index
    $k = [Math]::Floor($Year / 100)

    # Shift of metonic cycle, add a day offset every 300 years
    $p = [Math]::Floor((13 + 8 * $k) / 25)

    # Correction for non-observed leap days
    $q = [Math]::Floor($k / 4)

    # Correction to starting point of calculation each century
    $M = (15 - $p + $k - $q) % 30

    # Number of days from March 21st until the full moon
    $d = (19 * $a + $M) % 30

    # Returning if user wants value for Servois' table
    if($Servois) {
        return ((21 + $d) % 31).ToString()
    }

    # Finding the next Sunday
    # Century-based offset in weekly calculation
    $N = (4 + $k - $q) % 7

    # Correction for leap days
    $b = $Year % 4
    $c = $Year % 7

    # Days from d to next Sunday
    $e = (2 * $b + 4 * $c + 6 * $d + $N) % 7

    # Historical corrections for April 26 and 25
    if(($d -eq 29 -and $e -eq 6) -or ($d -eq 28 -and $e -eq 6 -and $a -gt 10)) {
        $e = -1
    }

    # Determination of the correct month for Easter
    if(22 + $d + $e -gt 31) {
        return "April " + ($d + $e - 9)
    }
    else {
        return "March " + (22 + $d + $e)
    }
}


# Here, we will output the date of the Paschal full moon
# (using Servois notation), and Easter for 2020-2030

Write-Host "The following are the dates of the Paschal full moon (using Servois",
           "notation) and the date of Easter for 2020-2030 AD:"
Write-Host "Year`tServois number`tEaster"
foreach($year in 2020..2030) {
    Write-Host "$year`t$(Calculate-Computus $year -Servois)`t`t$(Calculate-Computus $year)"
}