object GiftWrap {

  case class Point(x: Int, y: Int)

  def jarvisMarch(gift: List[Point]): List[Point] = {

    def isClockwise(p1: Point, p2: Point, p3: Point): Boolean =
      (p3.y - p1.y) * (p2.x - p1.x) >= (p2.y - p1.y) * (p3.x - p1.x)

    def leastClockwise(apex: Point)(a: Point, b: Point): Point =
      if (!isClockwise(apex, a, b)) a else b

    def nextPointOnHull(current: Point): Point =
      gift
        .filter(_ != current) //ignore current point
        .reduce(leastClockwise(current))

    def leftmost(points: List[Point]): Point =
      points.reduce((a, b) => if (a.x < b.x) a else b)

    def buildHull(hull: List[Point]): List[Point] =
      nextPointOnHull(hull.last) match {
        case backToStart if backToStart == hull.head => hull
        case nextPoint                               => buildHull(hull :+ nextPoint)
      }

    buildHull(List(leftmost(gift))) // leftmost point guaranteed to be in hull

  }

  def main(args: Array[String]): Unit = {

    val testGift = List(
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

    val hull = jarvisMarch(testGift)

    println("The points in the wrapping are:")
    hull.foreach(println)
  }
}
