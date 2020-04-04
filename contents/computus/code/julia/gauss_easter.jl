function computus(year)

    # Year's position on the 19 year metonic cycle
    a = mod(year, 19)
    
    # Century index
    k = floor(year/100)

    # shift of metonic cycle, add a day offset every 300 years
    # 8/25 is roughly 1/3rd, so every 300 years, that will add a day.
    # 13 is an offset.
    # 8 days off every 2500 years.
    # This might be why the alg. breaks down at 4200
    p = floor((13 + 8 * k) / 25) 

    # Every 400 years, there are 97 leap days. k is already a century index,
    # so every 400 years, it will be 4. q will then subtract 1 from that and 
    # give us 3, so that 100-3=97
    # (k-q) should be thought of as its own variable.
    q = floor(k / 4)

    # Correction to starting point of calculation each century, mod 30 because
    # the lunar calendar is about 30. This is a combination of previous terms.
    M = mod(15 - p + k - q, 30)

    # Number of days from March 21st until the full moon
    # moon year = 12 lunar months = 354 days = 365 - 11 and mod(-11, 30) = 19
    # 0.25 year offset is handled by leap days later
    d = mod(19 * a + M, 30)

    # Finding the next Sunday
    # The difference in the number of leap days between the Gregorian and
    # Julian calendars. 4 = Number of leap days per year. 4 is an offset.
    # k-q is the number of additional leap days, and mod 7 because week
    N = mod(4 + k - q, 7)

    # 52x7 = 364, but there are 365 days a year, so we are offsetting for this.
    # 2*b + 4*c  will drop by 1 every year, but 2 on leap years
    b = mod(year, 4)
    c = mod(year, 7)

    # Ignoring the 6*d term, this is the number of days until the next sunday
    # from March 22. The 6*d adds the appropriate offset to the next Sunday.
    e = mod(2 * b + 4 * c + 6 * d + N, 7)

    # Historical corrections for April 26 and 25 if we are on the last 8 years
    # of the metonic cycle. There is some ambiguity here I could not understand
    # related to the length of each lunar month being ~29.5 days, which could 
    # mean that this is actually a mathematical offset.
    # Could be an equinox correction where the second half of the metonic cycle
    # has an earlier equinox?
    if (d == 29 && e == 6) || (d == 28 && e == 6 && a > 10)
        e = -1
    end

    # uncomment to recreate Servois's table
    return mod(21 + d, 31)
    if(22+ d + e > 31)
        return string(d + e - 9)*" April"
    else
        return string(22+d+e)*" March"
    end
end

easter = computus(1800)
