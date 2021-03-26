object GiftWrap {

  case class Point(x: Int, y: Int)

  def jarvis_march(gift: List[Point]): List[Point] = {

    def is_clockwise(p1: Point, p2: Point, p3: Point): Boolean =
      (p3.y - p1.y) * (p2.x - p1.x) >= (p2.y - p1.y) * (p3.x - p1.x)

    def most_clockwise(apex: Point)(a: Point, b: Point): Point =
          if (!is_clockwise(apex, a, b)) a else b

    def next_point_on_hull(current: Point): Point =
      gift
        .filter(_ != current) //ignore current point
        .reduce(most_clockwise(current))

    def build_hull(hull: List[Point]): List[Point] = 
      next_point_on_hull(hull.last) match {
        case back_to_start if back_to_start == hull.head => hull
        case new_point => build_hull(hull :+ new_point)
      }

    def leftmost(points: List[Point]): Point = 
      points.reduce((a, b) => if (a.x < b.x) a else b) 
      
    // leftmost point guaranteed to be in hull
    build_hull(List(leftmost(gift)))

  }


  def main(args: Array[String]): Unit = {

    val test_gift = List(
      (-5, 2),
      (-13, 100),
      (5, 7),
      (-6, -12),
      (-14, -14),
      (9, 9),
      (-1, -1),
      (100, 100),
      (-10, 11),
      (-13, 100),
      (-6, 15),
      (-6, -8),
      (15, -9),
      (7, -7),
      (-2, -9),
      (100, -100),
      (6, -5),
      (0, 14),
      (2, 8)
    ).map({ case (x, y) => Point(x, y) })

    val hull = jarvis_march(test_gift)

    println("The points in the wrapping are:")
    hull.foreach(println)

  }
}