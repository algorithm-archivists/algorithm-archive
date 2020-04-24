object BubbleSort {

  def bubbleDown(list: List[Int]): List[Int] =
    list match {
      case a :: b :: tail if a < b => b :: bubbleDown(a :: tail)
      case a :: b :: tail => a :: bubbleDown(b :: tail)
      case _ => list
    }

  def bubbleSort(list: List[Int]): List[Int] =
    bubbleDown(list) match {
      case unsorted :+ smallest => smallest :: bubbleDown(unsorted)
      case _ => list
    }

  def main(args: Array[String]): Unit = {
    val unsorted = List(9, 2, 0, 5, 3, 8, 1, 9, 4, 0, 7, 0, 9, 9, 0)

    println("Unsorted list is " + unsorted)
    println("  Sorted list is " + bubbleSort(unsorted))
  }
}
