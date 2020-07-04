function computus(year; servois=false)

    # Year's position on the 19 year metonic cycle
    a = mod(year, 19)

    # Century index
    k = fld(year, 100)

    # Shift of metonic cycle, add a day offset every 300 years
    p = fld(13 + 8 * k, 25)

    # Correction for non-observed leap days
    q = fld(k, 4)

    # Correction to starting point of calculation each century
    M = mod(15 - p + k - q, 30)

    # Number of days from March 21st until the full moon
    d = mod(19 * a + M, 30)

    # Returning if user wants value for Servois' table
    if servois
        return string(mod(21 + d,31))
    end

    # Finding the next Sunday
    # Century-based offset in weekly calculation
    N = mod(4 + k - q, 7)

    # Correction for leap days
    b = mod(year, 4)
    c = mod(year, 7)

    # Days from d to next Sunday
    e = mod(2 * b + 4 * c + 6 * d + N, 7)

    # Historical corrections for April 26 and 25
    if (d == 29 && e == 6) || (d == 28 && e == 6 && a > 10)
        e = -1
    end

    # Determination of the correct month for Easter
    if(22 + d + e > 31)
        return "April " * string(d + e - 9)
    else
        return "March " * string(22 + d + e)
    end
end

# Here, we will output the date of the Paschal full moon
# (using Servois notation), and Easter for 2020-2030

a = collect(2020:2030)
servois_numbers = computus.(a; servois=true)
easter_dates = computus.(a)

println("The following are the dates of the Paschal full moon (using Servois " *
        "notation) and the date of Easter for 2020-2030 AD:")
println("Year\tServois number\tEaster")
for i = 1:length(a)
    println("$(a[i])\t$(servois_numbers[i])\t\t$(easter_dates[i])")
end
