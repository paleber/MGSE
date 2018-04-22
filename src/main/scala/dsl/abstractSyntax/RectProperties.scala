package dsl.abstractSyntax

trait RectProperties extends Element {

  val dimension: (Int, Int)
  val position: Option[(Int, Int)]
  val rounded: Option[(Int, Int)]
  val filled: Option[String]
  val stroked: Option[(String, Int)]

}


trait NoPosition {

  def atPosition(x: Int, y: Int): RectProperties

}

trait NotRounded {

  def rounded(radius: Int): RectProperties

  def rounded(radiusX: Int, radiusY: Int): RectProperties

}

trait NotFilled {

  def filled(color: String): RectProperties

}

trait NotStroked {

  def stroked(color: String, width: Int): RectProperties

}
