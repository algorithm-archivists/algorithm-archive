import strformat

func computus(year: int, servois: bool = false): string =
  let
    # Year's position on the 19 year metonic cycle
    a = year mod 19
    # Century index
    k = year div 100
    # Shift of metonic cycle, add a day offset every 300 years
    p = (13 + 8 * k) div 25
    # Correction for non-observed leap days
    q = k div 4
    # Correction to starting point of calculation each century
    m = (15 - p + k - q) mod 30
    # Number of days from March 21st until the full moon
    d = (19 * a + m) mod 30
  # Returning of user wants value for Servois' table
  if servois:
    return $((21 + d) mod 31)
  let
    # Find the next Sunday
    # Century-based offset in weekly calculation
    n = (4 + k - q) mod 7
    # Correction for leap days
    b = year mod 4
    c = year mod 7
    # Days from d to next Sunday
    temp_e = (2 * b + 4 * c + 6 * d + n) mod 7
    # Historical corrections for April 26 and 25
    e = if (d == 29 and temp_e == 6) or (d == 28 and temp_e == 6 and a > 10):
          -1
        else:
          temp_e
  # Determination of the correct month for Easter
  if (22 + d + e) > 31:
    result = "April {d + e - 9}".fmt
  else:
    result = "March {22 + d + e}".fmt

when isMainModule:
  echo "The following are the dates of the Paschal full moon (using Servois "
  echo "notation) and the date of Easter for 2020-2030 AD:"
  echo "Year Servois number Easter"
  for year in 2020..2030:
    echo "{year} {computus(year, true):14} {computus(year, false):6}".fmt
