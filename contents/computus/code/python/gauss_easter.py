def computus(year, servois=False):
    # Year's position on the 19-year metonic cycle
    a = year % 19

    # Century index
    k = year // 100

    # Shift of metonic cycle, add a day offset every 300 years
    p = (13 + 8 * k) // 25

    # Correction for non-observed leap days
    q = k // 4

    # Correction to starting point of calculation each century
    M = (15 - p + k - q) % 30

    # Number of days from March 21st until the full moon
    d = (19 * a + M) % 30

    # Returning if user wants value for Servois' table
    if servois:
        return str((21 + d) % 31)

    # Finding the next Sunday
    # Century-based offset in weekly calculation
    N = (4 + k - q) % 7

    # Correction for leap days
    b = year % 4
    c = year % 7

    # Days from d to next Sunday
    e = (2 * b + 4 * c + 6 * d + N) % 7

    # Historical corrections for April 26 and 25
    if (d == 29 and e == 6) or (d == 28 and e == 6 and a > 10):
        e = -1

    # Determination of the correct month for Easter
    if 22 + d + e > 31:
        return "April " + str(d + e - 9)
    else:
        return "March " + str(22 + d + e)


# Here, we will output the date of the Paschal full moon
# (using Servois notation), and Easter for 2020-2030

print(
    "The following are the dates of the Paschal full moon (using Servois",
    "notation) and the date of Easter for 2020-2030 AD:",
)
print("Year\tServois number\tEaster")
for year in range(2020, 2031):
    print(f"{year}\t{computus(year, servois=True)}\t\t{computus(year)}")
