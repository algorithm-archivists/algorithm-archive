String computus(int year, {bool servois = false}) {
  // Year's position in metonic cycle
  final a = year % 19;

  // Century index
  final k = (year / 100).floor();

  // Shift of metonic cycle, add a day offset every 300 years
  final p = ((13 + 8 * k) / 25).floor();

  // Correction for non-observed leap days
  final q = (k / 4).floor();

  // Correction to starting point of calculation each century
  final M = (15 - p + k - q) % 30;

  // Number of days from March 21st until the full moon
  final d = (19 * a + M) % 30;

  // Returning if user wants value for Servois' table
  if (servois) {
    return ((21 + d) % 31).toString();
  }

  // Finding the next Sunday
  // Century-based offset in weekly calculation
  final N = (4 + k - q) % 7;

  // Correction for leap days
  final b = year % 4;
  final c = year % 7;

  // Days from d to next Sunday
  var e = (2 * b + 4 * c + 6 * d + N) % 7;

  // Historical corrections for April 26 and 25
  if (e == 6) {
    if (d == 29 || (d == 28 && a > 10)) {
      e = -1;
    }
  }

  // Determination of the correct month for Easter
  if (22 + d + e > 31) {
    return 'April ${d + e - 9}';
  } else {
    return 'March ${22 + d + e}';
  }
}

void main() {
  print("The following are the dates of the Paschal full moon (using Servois " +
      "notation) and the date of Easter for 2020-2030 AD:");

  print("Year\tServois number\tEaster");

  for (var year = 2020; year <= 2030; year++) {
    final servoisNumber = computus(year, servois: true);
    final easterDate = computus(year);

    print('$year\t$servoisNumber\t\t$easterDate');
  }
}
