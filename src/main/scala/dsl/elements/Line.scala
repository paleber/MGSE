package dsl.elements

import dsl.abstractSyntax.{LineNotStroked, LineProperties, NoEndPoint, NoStartPoint}


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
                        stroked: Option[(String, Int)]) extends LineProperties
