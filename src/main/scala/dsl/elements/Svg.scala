package dsl.elements

import dsl.abstractSyntax.{Element, NoHeight, NoWidth, SvgProperties}


object Svg extends SvgProperties {

  override val width: Option[Int] = None
  override val height: Option[Int] = None
  override val elements: List[Element] = Nil

  def ofWidth(width: Int) = SvgNoHeight(Some(width), Nil)

  def ofHeight(height: Int) = SvgNoWidth(Some(height), Nil)

  def withElement(element: Element) = SvgNoWidthNoHeight(List(element))

}


sealed case class SvgNoWidthNoHeight(elements: List[Element]) extends SvgProperties with NoWidth with NoHeight {

  override val width: Option[Int] = None
  override val height: Option[Int] = None

  override def ofWidth(width: Int) = SvgNoHeight(Some(width), elements)

  override def ofHeight(height: Int) = SvgNoWidth(Some(height), elements)

  override def withElement(element: Element): SvgNoWidthNoHeight = copy(element :: elements)

}


sealed case class SvgNoWidth(height: Option[Int], elements: List[Element]) extends SvgProperties with NoWidth {

  override val width: Option[Int] = None

  override def ofWidth(width: Int): SvgProperties = CompleteSvg(Some(width), height, elements)

  override def withElement(element: Element): SvgNoWidth = copy(height, element :: elements)

}


sealed case class SvgNoHeight(width: Option[Int], elements: List[Element]) extends SvgProperties with NoHeight {

  override val height: Option[Int] = None

  override def ofHeight(height: Int) = CompleteSvg(width, Some(height), elements)

  override def withElement(element: Element): SvgNoHeight = copy(width, element :: elements)

}


sealed case class CompleteSvg(width: Option[Int], height: Option[Int], elements: List[Element]) extends SvgProperties {

  override def withElement(element: Element): CompleteSvg = copy(width, height, element :: elements)

}
