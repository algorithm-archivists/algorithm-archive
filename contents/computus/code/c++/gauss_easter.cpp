#include <iostream>
#include <string>

std::string computus(int year, bool servois = false) {
  // Year's position on the 19 year metonic cycle
  int a = year % 19;

  // Century index
  int k = year / 100;

  // Shift of metonic cycle, add a day offset every 300 years
  int p = (13 + 8 * k) / 25;

  // Correction for non-observed leap days
  int q = k / 4;

  // Correction to starting point of calculation each century
  int M = (15 - p + k - q) % 30;

  // Number of days from March 21st until the full moon
  int d = (19 * a + M) % 30;

  // Returning if user wants value for Servois' table
  if (servois) {
    return std::to_string((21 + d) % 31);
  }

  // Finding the next Sunday
  // Century-based offset in weekly calculation
  int N = (4 + k - q) % 7;

  // Correction for leap days
  int b = year % 4;
  int c = year % 7;

  // Days from d to next Sunday
  int e = (2 * b + 4 * c + 6 * d + N) % 7;

  // Historical corrections for April 26 and 25
  if ((d == 29 && e == 6) || (d == 28 && e == 6 && a > 10)) {
    e = -1;
  }

  // Determination of the correct month for Easter
  return 22 + d + e > 31 ? "April " + std::to_string(d + e - 9)
                         : "March " + std::to_string(22 + d + e);
}

// Here, we will output the date of the Paschal full moon (using Servois
// notation), and Easter for 2020-2030
int main() {
  std::cout << "The following are the dates of the Paschal full moon (using "
               "Servois notation) and the date of Easter for 2020-2030 AD:\n"
               "Year\tServois number\tEaster\n";

  for (int year = 2020; year <= 2030; year++) {
    std::cout << year << "\t\t" << computus(year, true) << '\t'
              << computus(year) << std::endl;
  }
}
