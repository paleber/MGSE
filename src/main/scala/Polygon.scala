

trait PolygonProperties extends Element {

  val points: List[(Int, Int)]
  val filled: Option[String]
  val stroked: Option[(String, Int)]

  def nextPoint(x: Int, y: Int): PolygonProperties

  override def toXml: String = {


    val filledAttribute = filled.fold("") { f =>
      s"""
         |    fill="$f"""".stripMargin
    }

    val strokeAttribute = stroked.fold("") { s =>
      s"""
         |    stroke="${s._1}"
         |    stroke-width="${s._2}"""".stripMargin
    }

    val pointsAttribute =
      s"""points="${points.map{ case(x, y) =>
        s"$x,$y"
      }.mkString(" ")}""""

    s"""<polygon $pointsAttribute$filledAttribute$strokeAttribute />""".stripMargin
  }

}

trait PolygonNotFilled {

  def filled(color: String): PolygonProperties

}

trait PolygonNotStroked {

  def stroked(color: String, width: Int): PolygonProperties

}

trait PolygonExtended {
  val previousPoints: List[(Int, Int)]
  val point: (Int, Int)

  val points: List[(Int, Int)] = point :: previousPoints
}

object Polygon {

  def firstPoint(x: Int, y: Int) = PolygonOnePointed((x, y))

}

case class PolygonOnePointed(firstPoint: (Int, Int)) {

  val points = List(firstPoint)

  def nextPoint(x: Int, y: Int) = PolygonTwoPointed(points, (x, y))
}

case class PolygonTwoPointed(previousPoints: List[(Int, Int)], secondPoint: (Int, Int)) {
  val points: List[(Int, Int)] = secondPoint :: previousPoints

  def nextPoint(x: Int, y: Int) = PolygonValid(points, (x, y))
}

case class PolygonValid(previousPoints: List[(Int, Int)], thridPoint: (Int, Int)) extends PolygonProperties with PolygonNotFilled with PolygonNotStroked {

  override val points: List[(Int, Int)] = thridPoint :: previousPoints
  override val filled: Option[String] = None
  override val stroked: Option[(String, Int)] = None

  override def nextPoint(x: Int, y: Int) = PolygonSimpleExtended(points, (x, y))

  override def stroked(color: String, width: Int) = PolygonStroked(points, Some((color, width)))

  override def filled(color: String) = PolygonFilled(points, Some(color))

}

case class PolygonSimpleExtended(previousPoints: List[(Int, Int)],
                                 point: (Int, Int)) extends PolygonProperties with PolygonNotFilled with PolygonNotStroked with PolygonExtended {

  override val filled: Option[String] = None
  override val stroked: Option[(String, Int)] = None

  override def nextPoint(x: Int, y: Int) = PolygonSimpleExtended(points, (x, y))

  override def stroked(color: String, width: Int) = PolygonStroked(points, Some((color, width)))

  override def filled(color: String) = PolygonFilled(points, Some(color))


}

case class PolygonFilled(points: List[(Int, Int)], filled: Option[String]) extends PolygonProperties with PolygonNotStroked {

  override val stroked: Option[(String, Int)] = None

  override def nextPoint(x: Int, y: Int) = PolygonFilledExtended(points, (x, y), filled)

  override def stroked(color: String, width: Int) = PolygonComplete(points, filled, Some((color, width)))

}

case class PolygonFilledExtended(previousPoints: List[(Int, Int)],
                                 point: (Int, Int),
                                 filled: Option[String]) extends PolygonProperties with PolygonNotStroked with PolygonExtended {

  override val stroked: Option[(String, Int)] = None

  override def nextPoint(x: Int, y: Int) = PolygonFilledExtended(points, (x, y), filled)

  override def stroked(color: String, width: Int) = PolygonComplete(points, filled, Some((color, width)))

}


case class PolygonStroked(points: List[(Int, Int)], stroked: Option[(String, Int)]) extends PolygonProperties with PolygonNotFilled {

  override val filled: Option[String] = None

  override def nextPoint(x: Int, y: Int) = PolygonStrokedExtended(points, (x, y), stroked)

  override def filled(color: String) = PolygonComplete(points, Some(color), stroked)

}

case class PolygonStrokedExtended(previousPoints: List[(Int, Int)],
                                  point: (Int, Int),
                                  stroked: Option[(String, Int)]) extends PolygonProperties with PolygonNotFilled with PolygonExtended {

  override val filled: Option[String] = None

  override def nextPoint(x: Int, y: Int) = PolygonStrokedExtended(points, (x, y), stroked)

  override def filled(color: String): PolygonProperties = PolygonComplete(points, Some(color), stroked)

}

case class PolygonComplete(points: List[(Int, Int)],
                           filled: Option[String],
                           stroked: Option[(String, Int)]) extends PolygonProperties {

  override def nextPoint(x: Int, y: Int) = PolygonCompleteExtended(points, (x, y), filled, stroked)
}

case class PolygonCompleteExtended(previousPoints: List[(Int, Int)],
                                   point: (Int, Int),
                                   filled: Option[String],
                                   stroked: Option[(String, Int)]) extends PolygonProperties with PolygonExtended {

  override val points: List[(Int, Int)] = point :: previousPoints

  override def nextPoint(x: Int, y: Int) = PolygonCompleteExtended(points, (x, y), filled, stroked)
}