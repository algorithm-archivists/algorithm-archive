fn computus(year: usize, servois: bool) -> String {
    // Year's position on the 19 year metonic cycle
    let a = year % 19;

    // Century index
    let k = year / 100; // NOTE: dividing integers always truncates the result

    // Shift of metonic cycle, add a day offset every 300 years
    let p = (13 + 8 * k) / 25;

    // Correction for non-observed leap days
    let q = k / 4;

    // Correction to starting point of calculation each century
    let m = (15 - p + k - q) % 30;

    // Number of days from March 21st until the full moon
    let d = (19 * a + m) % 30;

    if servois {
        return ((21 + d) % 31).to_string();
    }

    // Finding the next Sunday
    // Century-based offset in weekly calculation
    let n = (4 + k - q) % 7;

    // Correction for leap days
    let b = year % 4;
    let c = year % 7;

    // Days from d to next Sunday
    let temp_e = ((2 * b + 4 * c + 6 * d + n) % 7) as isize;

    // Historical corrections for April 26 and 25
    let e = if (d == 29 && temp_e == 6) || (d == 28 && temp_e == 6 && a > 10) {
        -1
    } else {
        temp_e
    };

    // Determination of the correct month for Easter
    if (22 + d) as isize + e > 31 {
        format!("April {}", d as isize + e - 9)
    } else {
        format!("March {}", 22 + d as isize + e)
    }
}

fn main() {
    // Here, we will output the date of the Paschal full moon
    // (using Servois notation), and Easter for 2020-2030

    let years = 2020..=2030;

    println!(
        "The following are the dates of the Paschal full moon (using \
            Servois notation) and the date of Easter for 2020-2030 AD:"
    );
    println!("Year\tServois number\tEaster");
    years.for_each(|year| {
        println!(
            "{}\t{:<14}\t{}",
            year,
            computus(year, true),
            computus(year, false),
        )
    });
}
