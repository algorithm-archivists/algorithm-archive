/**
 * In this code, the modulus operator is used.
 * However, this operator in javascript/typescript doesn't support negative numbers.
 * So, where there may be negative numbers, the function mod is used.
 * This function gives the modulo of any relative number a
 */

/**
 * @param {number} a
 * @param {number} b
 * @returns {number}
 */
function mod(a, b) {
  if (a < 0) return mod(a + b, b);
  else return a % b;
}

/**
 * @param {number} year
 * @param {boolean} [servois=false]
 * @returns {string}
 */
function computus(year, servois = false) {
  // Year's position in metonic cycle
  const a = year % 19;

  // Century index
  const k = Math.floor(year / 100);

  // Shift of metonic cycle, add a day offset every 300 years
  const p = Math.floor((13 + 8 * k) / 25);

  // Correction for non-observed leap days
  const q = Math.floor(k / 4);

  // Correction to starting point of calculation each century
  const M = mod(15 - p + k - q, 30);

  // Number of days from March 21st until the full moon
  const d = (19 * a + M) % 30;

  // Returning if user wants value for Servois' table
  if (servois) {
    return ((21 + d) % 31).toString();
  }

  // Finding the next Sunday
  // Century-based offset in weekly calculation
  const N = mod(4 + k - q, 7);

  // Correction for leap days
  const b = year % 4;
  const c = year % 7;

  // Days from d to next Sunday
  let e = (2 * b + 4 * c + 6 * d + N) % 7;

  // Historical corrections for April 26 and 25
  if (e === 6) {
    if (d === 29 || (d === 28 && a > 10)) {
      e = -1;
    }
  }

  // Determination of the correct month for Easter
  if (22 + d + e > 31) {
    return `April ${d + e - 9}`;
  } else {
    return `March ${22 + d + e}`;
  }
}

console.log(
  "The following are the dates of the Paschal full moon (using Servois " +
    "notation) and the date of Easter for 2020-2030 AD:"
);

const values = [];

for (let year = 2020; year <= 2030; year++) {
  const servoisNumber = computus(year, true);
  const easterDate = computus(year);

  // Creation of an object to be displayed as a line in the output table
  values[year] = {
    "servois number": +servoisNumber,
    easter: easterDate,
  };
}

console.table(values);
