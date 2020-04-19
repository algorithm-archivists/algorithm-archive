import scala.util.Random.shuffle

object BogoSort {

  def isSorted(list: List[Int]): Boolean =
    list match {
      case Nil => true
      case a :: b :: _ if a > b => false
      case _ :: tail => isSorted(tail)
    }

  def bogoSort(list: List[Int]): List[Int] =
    isSorted(list) match {
      case false => bogoSort(shuffle(list))
      case _ => list
    }

  def main(args: Array[String]): Unit = {
    val unsorted = List(5, 2, 7, 1, -5)
    
    println("Unsorted list is " + unsorted)
    println("  Sorted list is " + bogoSort(unsorted))
  }

}
