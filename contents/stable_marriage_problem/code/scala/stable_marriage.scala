import scala.collection.mutable._

object StableMarriage {

  var bachelors = Queue[Man]()

  case class Man(name: String, var preferences: List[Woman] = List()) {
    def propose(): Unit = preferences match {
      case woman :: remainingPreferences => {
        if (woman.prefers(this)) {
          bachelors ++= woman.fiance
          woman.fiance = Some(this)
        }
        else
          bachelors.enqueue(this)
        preferences = remainingPreferences
      }
      case _ =>
    }
  }

  case class Woman(name: String, var preferences: List[Man] = List(), var fiance: Option[Man] = None) {
    def prefers(man: Man): Boolean =
      fiance match {
        case Some(otherMan) => preferences.indexOf(man) < preferences.indexOf(otherMan)
        case _ => true //always prefer any man over nobody
      }
  }

  def findStableMatches(men: Man*): Unit = {
    bachelors = men.to[Queue]
    while (bachelors.nonEmpty)
      bachelors.dequeue.propose()
  }
}

object StableMarriageExample {

  val a = StableMarriage.Man("Adam")
  val b = StableMarriage.Man("Bart")
  val c = StableMarriage.Man("Colm")
  val x = StableMarriage.Woman("Xena")
  val y = StableMarriage.Woman("Yeva")
  val z = StableMarriage.Woman("Zara")

  a.preferences = List(y, x, z)
  b.preferences = List(y, z, x)
  c.preferences = List(x, z, y)
  x.preferences = List(b, a, c)
  y.preferences = List(c, a, b)
  z.preferences = List(a, c, b)


  def main(args: Array[String]): Unit = {

    StableMarriage.findStableMatches(a, b, c)

    List(x, y, z).foreach(
      w => Console.println(
        w.name
          + " is married to "
          + w.fiance.getOrElse(StableMarriage.Man("Nobody")).name))
  }

}
