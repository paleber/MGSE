
sealed trait SvgProperties {

  val width: Option[Int]
  val height: Option[Int]
  val elements: List[Element]

  def withElement(element: Element): SvgProperties

  def toXml: String = {

    val widthAttribute = width.fold("") { w =>
      s"""
         |  width="$w"""".stripMargin
    }

    val heightAttribute = height.fold("") { h =>
      s"""
         |  height="$h"""".stripMargin
    }

    val children = elements.map(x =>
      s"""
         |  ${x.toXml}
      """.stripMargin)

    s"""<svg xmlns="http://www.w3.org/2000/svg"$widthAttribute$heightAttribute>
       |${children.mkString}
       |</svg>
    """.stripMargin

  }

}

sealed trait NoWidth {

  def ofWidth(width: Int): SvgProperties

}

sealed trait NoHeight {

  def ofHeight(height: Int): SvgProperties

}

trait Element {

  def toXml: String

}



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

  override def withElement(element: Element): SvgNoHeight = copy(height, element :: elements)

}


sealed case class CompleteSvg(width: Option[Int], height: Option[Int], elements: List[Element]) extends SvgProperties {

  override def withElement(element: Element): CompleteSvg = copy(width, height, element :: elements)

}
