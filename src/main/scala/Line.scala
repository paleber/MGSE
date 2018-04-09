

trait LineProperties extends Element {

  val from: (Int, Int)
  val to: (Int, Int)
  val stroked: Option[(String, Int)]

  override def toXml: String = {
    val fromAttribute = {
      s"""
         |    x1="${from._1}"
         |    y1="${from._2}"""".stripMargin
    }

    val toAttribute = {
      s"""
         |    x2="${to._1}"
         |    y2="${to._2}"""".stripMargin
    }

    val strokeAttribute = stroked.fold("") { s =>
      s"""
         |    stroke="${s._1}"
         |    stroke-width="${s._2}"""".stripMargin
    }

    s"""<line $fromAttribute$toAttribute$strokeAttribute />""".stripMargin
  }


}

trait NoStartPoint {

  def from(x: Int, y: Int): LineWithStartPoint

}

trait NoEndPoint {

  def to(x: Int, y: Int): LineWithStartAndEndPoint

}

trait LineNotStroked {

  def stroked(color: String, width: Int): LineProperties

}


object Line extends NoStartPoint {

  def from(x: Int, y: Int) = LineWithStartPoint((x, y))
}


case class LineWithStartPoint(from: (Int, Int)) extends NoEndPoint {

  override def to(x: Int, y: Int) = LineWithStartAndEndPoint(from, (x, y))
}

case class LineWithStartAndEndPoint(from: (Int, Int),
                                    to: (Int, Int))
  extends LineProperties with LineNotStroked {

  override val stroked: Option[(String, Int)] = None

  override def stroked(color: String, width: Int) = CompleteLine(from, to, Some((color, width)))
}


case class CompleteLine(from: (Int, Int),
                        to: (Int, Int),
                        stroked: Option[(String, Int)]) extends LineProperties {}


