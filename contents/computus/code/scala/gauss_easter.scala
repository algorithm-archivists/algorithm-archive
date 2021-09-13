object GaussEaster {
  def computus(year : Int, servois: Boolean = false): String = {
    
    // Year's position on the 19 year metonic cycle
    val a = year % 19

    // Century index
    val k = (year / 100).toInt

    // Shift of metonic cycle, add a day offset every 300 years
    val p = ((13 + 8 * k) / 25).toInt

    // Correction for non-observed leap days
    val q = (k / 4).toInt

    // Correction to starting point of calculation each century
    val M = (15 - p + k - q) % 30

    // Number of days from March 21st until the full moon
    val d = (19 * a + M) % 30

    // Returning if user wants value for Servois' table
    if (servois) 
      return s"${(21 + d) % 31}"
    
    // Finding the next Sunday
    // Century-based offset in weekly calculation
    val N = (4 + k - q) % 7

    // Correction for leap days
    val b = year % 4
    val c = year % 7

    // Days from d to next Sunday
    var e = (2 * b + 4 * c + 6 * d + N) % 7

    // Historical corrections for April 26 and 25
    if ((d == 29 && e == 6) || (d == 28 && e == 6 && a > 10)) {
      e = -1
    }

    // Determination of the correct month for Easter
    if (22 + d + e > 31) 
      s"April ${d + e - 9}"
    else                 
      s"March ${22 + d + e}"
  }
  
  def main(args: Array[String]): Unit = {
    println("The following are the dates of the Paschal full moon (using " + 
            "Servois notation) and the date of Easter for 2020-2030 AD:\n" +
            "Year\tServois number\tEaster\n")
    
    for( year <- 2020 to 2030){
      println(s"$year \t\t ${computus(year, true)} \t${computus(year)}") 
    }
  }
}