import scala.collection.mutable._

object StableMarriage {

  case class Man(name: String, var preferences: List[Woman] = List()){
    def propose():Unit = preferences match {
      case woman :: remainingPreferences => {
        if (woman.prefers(this)) {
          bachelors ++= woman.fiance
          woman.fiance = Some(this)
        }
        else
          bachelors += this
        preferences = remainingPreferences
      }
      case _ =>
    }
  }

  case class Woman(name: String, var preferences: List[Man] = List(), var fiance: Option[Man] = None){
    def prefers(man: Man): Boolean =
      fiance match {
        case Some(otherMan) => preferences.indexOf(man) < preferences.indexOf(otherMan)
        case _ => true  //always prefer any man over nobody
      }
  }

  val a = Man("Adam")
  val b = Man("Bart")
  val c = Man("Colm")
  val x = Woman("Xena")
  val y = Woman("Yeva")
  val z = Woman("Zara")

  a.preferences = List(y, x, z)
  b.preferences = List(y, z, x)
  c.preferences = List(x, z, y)
  x.preferences = List(b, a, c)
  y.preferences = List(c, a, b)
  z.preferences = List(a, c, b)

  var bachelors = ListBuffer(a, b, c)

  def main(args: Array[String]): Unit = {

    bachelors foreach (b => b.propose())

    List(x, y, z).foreach(w => Console.println(w.name + " is married to " + w.fiance.getOrElse(Man("Nobody")).name))
  }
}
