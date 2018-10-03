object BubbleSort {

  def bubbleUpLargestToTheEnd(list: List[Int]): List[Int] =
    list match {
      case first :: second :: tail if first > second => second :: bubbleUpLargestToTheEnd(first :: tail)
      case first :: second :: tail => first :: bubbleUpLargestToTheEnd(second :: tail)
      case _ => list
    }

  def bubbleSort(list: List[Int]): List[Int] =
    bubbleUpLargestToTheEnd(list) match {
      case unsorted :+ largest => bubbleSort(unsorted) :+ largest
      case _ => list
    }


  def main(args: Array[String]): Unit = {
    val unsorted = List(9, 2, 0, 5, 3, 8, 1, 9, 4, 0, 7, 0, 9, 9, 0)

    println("Unsorted list is " + unsorted)
    println("  Sorted list is " + bubbleSort(unsorted))
  }
}
