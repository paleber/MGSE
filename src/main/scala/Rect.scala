

trait RectProperties extends Element {

  val dimension: (Int, Int)
  val position: Option[(Int, Int)]
  val rounded: Option[(Int, Int)]
  val filled: Option[String]
  val stroked: Option[(String, Int)]

  override def toXml: String =
    """<rect
      |  width="700"
      |  height="400"
      |  fill="yellow" />""".stripMargin

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


case class Rect(width: Int,
                height: Int)
  extends RectProperties with NoPosition with NotRounded with NotFilled with NotStroked {

  override val dimension: (Int, Int) = (width, height)
  override val position: Option[(Int, Int)] = None
  override val rounded: Option[(Int, Int)] = None
  override val filled: Option[String] = None
  override val stroked: Option[(String, Int)] = None

  override def atPosition(x: Int, y: Int) = RectNotRoundedNotFilledNotStroked(dimension, Some((x, y)))

  override def rounded(r: Int) = RectNoPositionNotFilledNotStroked(dimension, Some((r, r)))

  override def rounded(rx: Int, ry: Int) = RectNoPositionNotFilledNotStroked(dimension, Some((rx, ry)))

  override def filled(color: String) = RectNoPositionNotRoundedNotStroked(dimension, Some(color))

  override def stroked(color: String, width: Int) = RectNoPositionNotRoundedNotFilled(dimension, Some((color, width)))
}


case class RectNotRoundedNotFilledNotStroked(dimension: (Int, Int),
                                             position: Option[(Int, Int)])
  extends RectProperties with NotRounded with NotFilled with NotStroked {

  override val rounded: Option[(Int, Int)] = None
  override val filled: Option[String] = None
  override val stroked: Option[(String, Int)] = None

  override def rounded(r: Int) = RectNotFilledNotStroked(dimension, position, Some((r, r)))

  override def rounded(rx: Int, ry: Int) = RectNotFilledNotStroked(dimension, position, Some((rx, ry)))

  override def filled(color: String) = RectNotRoundedNotStroked(dimension, position, Some(color))

  override def stroked(color: String, width: Int) = RectNotRoundedNotFilled(dimension, position, Some((color, width)))

}


case class RectNoPositionNotFilledNotStroked(dimension: (Int, Int),
                                             rounded: Option[(Int, Int)])
  extends RectProperties with NoPosition with NotFilled with NotStroked {

  override val position: Option[(Int, Int)] = None
  override val filled: Option[String] = None
  override val stroked: Option[(String, Int)] = None

  override def atPosition(x: Int, y: Int) = RectNotFilledNotStroked(dimension, Some((x, y)), rounded)

  override def filled(color: String) = RectNoPositionNotStroked(dimension, rounded, Some(color))

  override def stroked(color: String, width: Int) = RectNoPositionNotFilled(dimension, rounded, Some((color, width)))
}


case class RectNoPositionNotRoundedNotStroked(dimension: (Int, Int),
                                              filled: Option[String])
  extends RectProperties with NoPosition with NotRounded with NotStroked {

  override val position: Option[(Int, Int)] = None
  override val rounded: Option[(Int, Int)] = None
  override val stroked: Option[(String, Int)] = None

  override def atPosition(x: Int, y: Int) = RectNotRoundedNotStroked(dimension, Some((x, y)), filled)

  override def rounded(r: Int) = RectNoPositionNotStroked(dimension, Some((r, r)), filled)

  override def rounded(rx: Int, ry: Int) = RectNoPositionNotStroked(dimension, Some((rx, ry)), filled)

  override def stroked(color: String, width: Int) = RectNoPositionNotRounded(dimension, filled, Some((color, width)))

}


case class RectNoPositionNotRoundedNotFilled(dimension: (Int, Int),
                                             stroked: Option[(String, Int)])
  extends RectProperties with NoPosition with NotRounded with NotFilled {

  override val position: Option[(Int, Int)] = None
  override val rounded: Option[(Int, Int)] = None
  override val filled: Option[String] = None

  override def atPosition(x: Int, y: Int) = RectNotRoundedNotFilled(dimension, Some((x, y)), stroked)

  override def rounded(r: Int) = RectNoPositionNotFilled(dimension, Some((r, r)), stroked)

  override def rounded(rx: Int, ry: Int) = RectNoPositionNotFilled(dimension, Some((rx, ry)), stroked)

  override def filled(color: String) = null //RectNoPositionNotRounded

}


case class RectNotFilledNotStroked(dimension: (Int, Int),
                                   position: Option[(Int, Int)],
                                   rounded: Option[(Int, Int)])
  extends RectProperties with NotFilled with NotStroked {

  override val filled: Option[String] = None
  override val stroked: Option[(String, Int)] = None

  override def filled(color: String) = null //RectNotStroked

  override def stroked(color: String, width: Int) = null//RectNotStroked
}


case class RectNotRoundedNotStroked(dimension: (Int, Int),
                                    position: Option[(Int, Int)],
                                    filled: Option[String])
  extends RectProperties with NotRounded with NotStroked {

  override val rounded: Option[(Int, Int)] = None
  override val stroked: Option[(String, Int)] = None

  override def rounded(r: Int) = null//RectNotStroked

  override def rounded(rx: Int, ry: Int) = null// RectNotStroked

  override def stroked(color: String, width: Int) = null//RectNotRounded

}


case class RectNotRoundedNotFilled(dimension: (Int, Int),
                                   position: Option[(Int, Int)],
                                   stroked: Option[(String, Int)])
  extends RectProperties with NotRounded with NotFilled {

  override val rounded: Option[(Int, Int)] = None
  override val filled: Option[String] = None

  override def rounded(r: Int) = null//RectNotFilled

  override def rounded(rx: Int, ry: Int) = null// RectNotFilled

  override def filled(color: String) = null//RectNotRounded

}


case class RectNoPositionNotStroked(dimension: (Int, Int),
                                    rounded: Option[(Int, Int)],
                                    filled: Option[String])
  extends RectProperties with NoPosition with NotStroked {

  override val position: Option[(Int, Int)] = None
  override val stroked: Option[(String, Int)] = None

  override def atPosition(x: Int, y: Int) = null// RectNotStroked

  override def stroked(color: String, width: Int) = null//RectNoPosition
}


case class RectNoPositionNotFilled(dimension: (Int, Int),
                                   rounded: Option[(Int, Int)],
                                   stroked: Option[(String, Int)])
  extends RectProperties with NoPosition with NotFilled {

  override val position: Option[(Int, Int)] = None
  override val filled: Option[String] = None

  override def atPosition(x: Int, y: Int) = null// RectNotFilled

  override def filled(color: String) = null//RectNoPosition

}


case class RectNoPositionNotRounded(dimension: (Int, Int),
                                    filled: Option[String],
                                    stroked: Option[(String, Int)])
  extends RectProperties with NoPosition with NotRounded {

  override val position: Option[(Int, Int)] = None
  override val rounded: Option[(Int, Int)] = None

  override def atPosition(x: Int, y: Int) = null//RectNotRounded

  override def rounded(r: Int) = null//RectNoPosition

  override def rounded(rx: Int, ry: Int) = null// RectNoPosition


}

