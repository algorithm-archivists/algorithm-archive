object BubbleSort {
  def bubbleBiggest(unprocessed: List[Int], notBiggest: List[Int] = List()): List[Int] =
    unprocessed match {
      case a :: b :: tail =>
        if (a > b) bubbleBiggest(a :: tail, b :: notBiggest)
        else bubbleBiggest(b :: tail, a :: notBiggest)
      case a :: Nil => a :: notBiggest
      case Nil => unprocessed
    }

  def bubbleSort(unsorted: List[Int], sorted: List[Int] = List()): List[Int] =
    bubbleBiggest(unsorted) match {
      case m :: us => bubbleSort(us, m :: sorted)
      case _ => sorted
    }

  def main(args: Array[String]): Unit =
    println(bubbleSort(List(9, 2, 0, 5, 3, 8, 1, 9, 4, 0, 7, 0, 9, 9, 0)))
}