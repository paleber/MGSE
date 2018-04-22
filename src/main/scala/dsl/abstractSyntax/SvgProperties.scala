package dsl.abstractSyntax

trait SvgProperties {

  val width: Option[Int]
  val height: Option[Int]
  val elements: List[Element]

  def withElement(element: Element): SvgProperties

}

trait NoWidth {

  def ofWidth(width: Int): SvgProperties

}

trait NoHeight {

  def ofHeight(height: Int): SvgProperties

}

trait Element {

}