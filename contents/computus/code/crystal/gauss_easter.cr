def computus(year, servois = false)
  # Year's position on the 19 year metonic cycle
  a = year % 19

  # Century index
  k = year // 100

  # Shift of metonic cycle, add a day offset every 300 years
  p = (13 + 8 * k) // 25

  # Correction for non-observed leap days
  q = k // 4

  # Correction to starting point of calculation each century
  m = (15 - p + k - q) % 30

  # Number of days from March 21st until the full moon
  d = (19 * a + m) % 30

  # Returning if user wants value for Servois' table
  if servois
    return ((21 + d) % 31).to_s
  end

  # Finding the next Sunday
  # Century-based offset in weekly calculation
  n = (4 + k - q) % 7

  # Correction for leap days
  b = year % 4
  c = year % 7

  # Days from d to next Sunday
  e = (2 * b + 4 * c + 6 * d + n) % 7

  # Historical corrections for April 26 and 25
  if (d == 29 && e == 6) || (d == 28 && e == 6 && a > 10)
    e = -1
  end

  # Determination of the correct month for Easter
  if (22 + d + e > 31)
    return "April " + (d + e - 9).to_s
  else
    return "March " + (22 + d + e).to_s
  end
end

# Here, we will output the date of the Paschal full moon
# (using Servois notation), and Easter for 2020-2030
def main
  a = (2020..2030).to_a
  servois_numbers = a.map { |y| computus(y, servois = true) }
  easter_dates = a.map { |y| computus(y) }

  puts "The following are the dates of the Paschal full moon (using Servois " +
       "notation) and the date of Easter for 2020-2030 AD:"
  puts "Year\tServois number\tEaster"
  a.each_index { |i|
    puts "#{a[i]}\t#{servois_numbers[i]}\t\t#{easter_dates[i]}"
  }
end

main
