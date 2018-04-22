package dsl.abstractSyntax

trait PolygonProperties extends Element {

  val points: List[(Int, Int)]
  val filled: Option[String]
  val stroked: Option[(String, Int)]
  val rotated: Option[(Int, Int, Int)]

  def nextPoint(x: Int, y: Int): PolygonProperties



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

