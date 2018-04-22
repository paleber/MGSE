package dsl.abstractSyntax

trait LineProperties extends Element {

  val from: (Int, Int)
  val to: (Int, Int)
  val stroked: Option[(String, Int)]



}

trait NoStartPoint {

  def from(x: Int, y: Int): NoEndPoint

}

trait NoEndPoint {

  def to(x: Int, y: Int): LineProperties

}

trait LineNotStroked {

  def stroked(color: String, width: Int): LineProperties

}
