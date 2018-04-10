

trait PolygonProperties extends Element {

  val points: List[(Int, Int)]
  val filled: Option[String]
  val stroked: Option[(String, Int)]
  val rotated: Option[(Int, Int, Int)]

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
      s"""points="${
        points.map { case (x, y) =>
          s"$x,$y"
        }.mkString(" ")
      }""""

    val rotatedAttribute = rotated.fold("") { r =>
      s"""
         |           transform="rotate(${r._1} ${r._2} ${r._3})"""".stripMargin
    }

    s"""<polygon $pointsAttribute$filledAttribute$strokeAttribute$rotatedAttribute />""".stripMargin
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

trait NotRotated {

  def rotate(x: Int, y: Int, z: Int): PolygonProperties
}

object Polygon {

  def firstPoint(x: Int, y: Int) = PolygonOnePointed((x, y))

}

case class PolygonOnePointed(firstPoint: (Int, Int)) {

  val points = List(firstPoint)

  def nextPoint(x: Int, y: Int) = PolygonTwoPointed(points, (x, y))
}

case class PolygonTwoPointed(previousPoints: List[(Int, Int)],
                             secondPoint: (Int, Int)) {
  val points: List[(Int, Int)] = secondPoint :: previousPoints

  def nextPoint(x: Int, y: Int) = PolygonValid(points, (x, y))
}

case class PolygonValid(previousPoints: List[(Int, Int)],
                        thridPoint: (Int, Int))
  extends PolygonProperties with PolygonNotFilled with PolygonNotStroked with NotRotated {

  override val points: List[(Int, Int)] = thridPoint :: previousPoints
  override val filled: Option[String] = None
  override val stroked: Option[(String, Int)] = None
  override val rotated: Option[(Int, Int, Int)] = None

  override def nextPoint(x: Int, y: Int) = PolygonSimpleExtended(points, (x, y))

  override def stroked(color: String, width: Int) = PolygonStroked(points, Some((color, width)))

  override def filled(color: String) = PolygonFilled(points, Some(color))

  override def rotate(x: Int, y: Int, z: Int) = PolygonRotated(points, Some((x, y, z)))
}

case class PolygonSimpleExtended(previousPoints: List[(Int, Int)],
                                 point: (Int, Int))
  extends PolygonProperties with PolygonNotFilled with PolygonNotStroked with PolygonExtended with NotRotated{

  override val filled: Option[String] = None
  override val stroked: Option[(String, Int)] = None
  override val rotated: Option[(Int, Int, Int)] = None

  override def nextPoint(x: Int, y: Int) = PolygonSimpleExtended(points, (x, y))

  override def stroked(color: String, width: Int) = PolygonStroked(points, Some((color, width)))

  override def filled(color: String) = PolygonFilled(points, Some(color))

  override def rotate(x: Int, y: Int, z: Int) = PolygonRotated(points, Some((x, y, z)))
}

case class PolygonRotated(points: List[(Int, Int)],
                          rotated: Option[(Int, Int, Int)])
  extends PolygonProperties with PolygonNotFilled with PolygonNotStroked {

  override val filled: Option[String] = None
  override val stroked: Option[(String, Int)] = None

  override def nextPoint(x: Int, y: Int) = PolygonRotatedExtended(points, (x, y), rotated)

  override def stroked(color: String, width: Int) = PolygonStroked(points, Some((color, width)))

  override def filled(color: String) = PolygonFilled(points, Some(color))

}

case class PolygonRotatedExtended(previousPoints: List[(Int, Int)],
                                  point: (Int, Int),
                                  rotated: Option[(Int, Int, Int)])
  extends PolygonProperties with PolygonNotFilled with PolygonNotStroked with PolygonExtended {

  override val filled: Option[String] = None
  override val stroked: Option[(String, Int)] = None

  override def nextPoint(x: Int, y: Int) = PolygonRotatedExtended(points, (x, y), rotated)

  override def stroked(color: String, width: Int) = PolygonStroked(points, Some((color, width)))

  override def filled(color: String) = PolygonFilled(points, Some(color))

}

case class PolygonFilled(points: List[(Int, Int)],
                         filled: Option[String])
  extends PolygonProperties with PolygonNotStroked with NotRotated {

  override val stroked: Option[(String, Int)] = None
  override val rotated: Option[(Int, Int, Int)] = None

  override def nextPoint(x: Int, y: Int) = PolygonFilledExtended(points, (x, y), filled)

  override def stroked(color: String, width: Int) = PolygonFilledStroked(points, filled, Some((color, width)))

  override def rotate(x: Int, y: Int, z: Int) = PolygonFilledRotated(points, filled, Some((x, y, z)))
}

case class PolygonFilledExtended(previousPoints: List[(Int, Int)],
                                 point: (Int, Int),
                                 filled: Option[String])
  extends PolygonProperties with PolygonNotStroked with PolygonExtended with NotRotated {

  override val stroked: Option[(String, Int)] = None
  override val rotated: Option[(Int, Int, Int)] = None

  override def nextPoint(x: Int, y: Int) = PolygonFilledExtended(points, (x, y), filled)

  override def stroked(color: String, width: Int) = PolygonFilledStroked(points, filled, Some((color, width)))

  override def rotate(x: Int, y: Int, z: Int) = PolygonFilledRotated(points, filled, Some((x, y, z)))

}

case class PolygonStroked(points: List[(Int, Int)],
                          stroked: Option[(String, Int)])
  extends PolygonProperties with PolygonNotFilled with NotRotated {

  override val filled: Option[String] = None
  override val rotated: Option[(Int, Int, Int)] = None

  override def nextPoint(x: Int, y: Int) = PolygonStrokedExtended(points, (x, y), stroked)

  override def filled(color: String) = PolygonFilledStroked(points, Some(color), stroked)

  override def rotate(x: Int, y: Int, z: Int) = PolygonStrokedRotated(points, stroked, Some((x, y, z)))
}

case class PolygonStrokedExtended(previousPoints: List[(Int, Int)],
                                  point: (Int, Int),
                                  stroked: Option[(String, Int)])
  extends PolygonProperties with PolygonNotFilled with NotRotated with PolygonExtended {

  override val filled: Option[String] = None
  override val rotated: Option[(Int, Int, Int)] = None

  override def nextPoint(x: Int, y: Int) = PolygonStrokedExtended(points, (x, y), stroked)

  override def filled(color: String): PolygonProperties = PolygonFilledStroked(points, Some(color), stroked)

  override def rotate(x: Int, y: Int, z: Int) = PolygonStrokedRotated(points, stroked, Some((x, y, z)))
}

case class PolygonFilledStroked(points: List[(Int, Int)],
                                filled: Option[String],
                                stroked: Option[(String, Int)])
  extends PolygonProperties with NotRotated {

  override val rotated: Option[(Int, Int, Int)] = None

  override def nextPoint(x: Int, y: Int) = PolygonFilledStrokedExtended(points, (x, y), filled, stroked)

  override def rotate(x: Int, y: Int, z: Int): PolygonProperties = PolygonComplete(points, filled, stroked, Some(x, y, z))
}

case class PolygonFilledStrokedExtended(previousPoints: List[(Int, Int)],
                                        point: (Int, Int),
                                        filled: Option[String],
                                        stroked: Option[(String, Int)])
  extends PolygonProperties with NotRotated with PolygonExtended {

  override val points: List[(Int, Int)] = point :: previousPoints
  override val rotated: Option[(Int, Int, Int)] = None

  override def nextPoint(x: Int, y: Int) = PolygonFilledStrokedExtended(points, (x, y), filled, stroked)

  override def rotate(x: Int, y: Int, z: Int) = PolygonComplete(points, filled, stroked, Some(x, y, z))
}


case class PolygonFilledRotated(points: List[(Int, Int)],
                                filled: Option[String],
                                rotated: Option[(Int, Int, Int)])
  extends PolygonProperties with PolygonNotStroked {

  override val stroked: Option[(String, Int)] = None

  override def nextPoint(x: Int, y: Int) = PolygonFilledRotatedExtended(points, (x, y), filled, rotated)

  override def stroked(color: String, width: Int) = PolygonComplete(points, filled, Some((color, width)), rotated)

}

case class PolygonFilledRotatedExtended(previousPoints: List[(Int, Int)],
                                        point: (Int, Int),
                                        filled: Option[String],
                                        rotated: Option[(Int, Int, Int)])
  extends PolygonProperties with PolygonNotStroked with PolygonExtended {

  override val stroked: Option[(String, Int)] = None

  override def nextPoint(x: Int, y: Int) = PolygonFilledRotatedExtended(points, (x, y), filled, rotated)

  override def stroked(color: String, width: Int) = PolygonComplete(points, filled, Some((color, width)), rotated)

}

case class PolygonStrokedRotated(points: List[(Int, Int)],
                                 stroked: Option[(String, Int)],
                                 rotated: Option[(Int, Int, Int)])
  extends PolygonProperties with PolygonNotFilled {

  override val filled: Option[String] = None

  override def nextPoint(x: Int, y: Int) = PolygonStrokedRotatedExtended(points, (x, y), stroked, rotated)

  override def filled(color: String) = PolygonComplete(points, Some(color), stroked, rotated)

}

case class PolygonStrokedRotatedExtended(previousPoints: List[(Int, Int)],
                                         point: (Int, Int),
                                         stroked: Option[(String, Int)],
                                         rotated: Option[(Int, Int, Int)])
  extends PolygonProperties with PolygonNotFilled with PolygonExtended {

  override val filled: Option[String] = None

  override def nextPoint(x: Int, y: Int) = PolygonStrokedRotatedExtended(points, (x, y), stroked, rotated)

  override def filled(color: String): PolygonProperties = PolygonComplete(points, Some(color), stroked, rotated)

}


case class PolygonComplete(points: List[(Int, Int)],
                           filled: Option[String],
                           stroked: Option[(String, Int)],
                           rotated: Option[(Int, Int, Int)])
  extends PolygonProperties {

  override def nextPoint(x: Int, y: Int) = PolygonCompleteExtended(points, (x, y), filled, stroked, rotated)

}

case class PolygonCompleteExtended(previousPoints: List[(Int, Int)],
                                   point: (Int, Int),
                                   filled: Option[String],
                                   stroked: Option[(String, Int)],
                                   rotated: Option[(Int, Int, Int)])
  extends PolygonProperties with PolygonExtended {

  override val points: List[(Int, Int)] = point :: previousPoints

  override def nextPoint(x: Int, y: Int) = PolygonCompleteExtended(points, (x, y), filled, stroked, rotated)

}