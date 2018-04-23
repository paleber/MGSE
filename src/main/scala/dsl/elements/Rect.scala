package dsl.elements

import dsl.abstractSyntax._


case class Rect(width: Int,
                height: Int)
  extends RectProperties with NoPosition with NotRounded with NotFilled with NotStroked {

  override val dimension: (Int, Int) = (width, height)
  override val position: Option[(Int, Int)] = None
  override val rounded: Option[(Int, Int)] = None
  override val filled: Option[String] = None
  override val stroked: Option[(String, Int)] = None

  override def atPosition(x: Int, y: Int) = RectPositioned(dimension, Some((x, y)))

  override def rounded(r: Int) = RectRounded(dimension, Some((r, r)))

  override def rounded(rx: Int, ry: Int) = RectRounded(dimension, Some((rx, ry)))

  override def filled(color: String) = RectFilled(dimension, Some(color))

  override def stroked(color: String, width: Int) = RectStroked(dimension, Some((color, width)))
}


case class RectPositioned(dimension: (Int, Int),
                          position: Option[(Int, Int)])
  extends RectProperties with NotRounded with NotFilled with NotStroked {

  override val rounded: Option[(Int, Int)] = None
  override val filled: Option[String] = None
  override val stroked: Option[(String, Int)] = None

  override def rounded(r: Int) = RectPositionedRounded(dimension, position, Some((r, r)))

  override def rounded(rx: Int, ry: Int) = RectPositionedRounded(dimension, position, Some((rx, ry)))

  override def filled(color: String) = RectPositionedFilled(dimension, position, Some(color))

  override def stroked(color: String, width: Int) = RectPositionedStroked(dimension, position, Some((color, width)))

}


case class RectRounded(dimension: (Int, Int),
                       rounded: Option[(Int, Int)])
  extends RectProperties with NoPosition with NotFilled with NotStroked {

  override val position: Option[(Int, Int)] = None
  override val filled: Option[String] = None
  override val stroked: Option[(String, Int)] = None

  override def atPosition(x: Int, y: Int) = RectPositionedRounded(dimension, Some((x, y)), rounded)

  override def filled(color: String) = RectRoundedFilled(dimension, rounded, Some(color))

  override def stroked(color: String, width: Int) = RectRoundedStroked(dimension, rounded, Some((color, width)))
}


case class RectFilled(dimension: (Int, Int),
                      filled: Option[String])
  extends RectProperties with NoPosition with NotRounded with NotStroked {

  override val position: Option[(Int, Int)] = None
  override val rounded: Option[(Int, Int)] = None
  override val stroked: Option[(String, Int)] = None

  override def atPosition(x: Int, y: Int) = RectPositionedFilled(dimension, Some((x, y)), filled)

  override def rounded(r: Int) = RectRoundedFilled(dimension, Some((r, r)), filled)

  override def rounded(rx: Int, ry: Int) = RectRoundedFilled(dimension, Some((rx, ry)), filled)

  override def stroked(color: String, width: Int) = RectFilledStroked(dimension, filled, Some((color, width)))

}


case class RectStroked(dimension: (Int, Int),
                       stroked: Option[(String, Int)])
  extends RectProperties with NoPosition with NotRounded with NotFilled {

  override val position: Option[(Int, Int)] = None
  override val rounded: Option[(Int, Int)] = None
  override val filled: Option[String] = None

  override def atPosition(x: Int, y: Int) = RectPositionedStroked(dimension, Some((x, y)), stroked)

  override def rounded(r: Int) = RectRoundedStroked(dimension, Some((r, r)), stroked)

  override def rounded(rx: Int, ry: Int) = RectRoundedStroked(dimension, Some((rx, ry)), stroked)

  override def filled(color: String) = RectFilledStroked(dimension, Some(color), stroked)

}


case class RectPositionedRounded(dimension: (Int, Int),
                                 position: Option[(Int, Int)],
                                 rounded: Option[(Int, Int)])
  extends RectProperties with NotFilled with NotStroked {

  override val filled: Option[String] = None
  override val stroked: Option[(String, Int)] = None

  override def filled(color: String) = RectPositionedRoundedFilled(dimension, position, rounded, Some(color))

  override def stroked(color: String, width: Int) = RectPositionedRoundedStroked(dimension, position, rounded, Some((color, width)))
}


case class RectPositionedFilled(dimension: (Int, Int),
                                position: Option[(Int, Int)],
                                filled: Option[String])
  extends RectProperties with NotRounded with NotStroked {

  override val rounded: Option[(Int, Int)] = None
  override val stroked: Option[(String, Int)] = None

  override def rounded(r: Int) = RectPositionedRoundedFilled(dimension, position, Some((r, r)), filled)

  override def rounded(rx: Int, ry: Int) = RectPositionedRoundedFilled(dimension, position, Some((rx, ry)), filled)

  override def stroked(color: String, width: Int) = RectPositionedFilledStroked(dimension, position, filled, Some((color, width)))

}


case class RectPositionedStroked(dimension: (Int, Int),
                                 position: Option[(Int, Int)],
                                 stroked: Option[(String, Int)])
  extends RectProperties with NotRounded with NotFilled {

  override val rounded: Option[(Int, Int)] = None
  override val filled: Option[String] = None

  override def rounded(r: Int) = RectPositionedRoundedStroked(dimension, position, Some((r, r)), stroked)

  override def rounded(rx: Int, ry: Int) = RectPositionedRoundedStroked(dimension, position, Some((rx, ry)), stroked)

  override def filled(color: String) = RectPositionedFilledStroked(dimension, position, Some(color), stroked)

}


case class RectRoundedFilled(dimension: (Int, Int),
                             rounded: Option[(Int, Int)],
                             filled: Option[String])
  extends RectProperties with NoPosition with NotStroked {

  override val position: Option[(Int, Int)] = None
  override val stroked: Option[(String, Int)] = None

  override def atPosition(x: Int, y: Int) = RectPositionedRoundedFilled(dimension, Some((x, y)), rounded, filled)

  override def stroked(color: String, width: Int) = RectRoundedFilledStroked(dimension, rounded, filled, Some((color, width)))

}


case class RectRoundedStroked(dimension: (Int, Int),
                              rounded: Option[(Int, Int)],
                              stroked: Option[(String, Int)])
  extends RectProperties with NoPosition with NotFilled {

  override val position: Option[(Int, Int)] = None
  override val filled: Option[String] = None

  override def atPosition(x: Int, y: Int) = RectPositionedRoundedStroked(dimension, Some((x, y)), rounded, stroked)

  override def filled(color: String) = RectRoundedFilledStroked(dimension, rounded, Some(color), stroked)

}


case class RectFilledStroked(dimension: (Int, Int),
                             filled: Option[String],
                             stroked: Option[(String, Int)])
  extends RectProperties with NoPosition with NotRounded {

  override val position: Option[(Int, Int)] = None
  override val rounded: Option[(Int, Int)] = None

  override def atPosition(x: Int, y: Int) = RectPositionedFilledStroked(dimension, Some((x, y)), filled, stroked)

  override def rounded(r: Int) = RectRoundedFilledStroked(dimension, Some((r, r)), filled, stroked)

  override def rounded(rx: Int, ry: Int) = RectRoundedFilledStroked(dimension, Some((rx, ry)), filled, stroked)

}


case class RectPositionedRoundedFilled(dimension: (Int, Int),
                                       position: Option[(Int, Int)],
                                       rounded: Option[(Int, Int)],
                                       filled: Option[String])
  extends RectProperties with NotStroked {

  override val stroked: Option[(String, Int)] = None

  override def stroked(color: String, width: Int) = RectComplete(dimension, position, rounded, filled, Some((color, width)))

}

case class RectPositionedRoundedStroked(dimension: (Int, Int),
                                        position: Option[(Int, Int)],
                                        rounded: Option[(Int, Int)],
                                        stroked: Option[(String, Int)])
  extends RectProperties with NotFilled {

  override val filled: Option[String] = None

  override def filled(color: String) = RectComplete(dimension, position, rounded, Some(color), stroked)

}


case class RectPositionedFilledStroked(dimension: (Int, Int),
                                       position: Option[(Int, Int)],
                                       filled: Option[String],
                                       stroked: Option[(String, Int)])
  extends RectProperties with NotRounded {

  override val rounded: Option[(Int, Int)] = None

  override def rounded(r: Int) = RectComplete(dimension, position, Some((r, r)), filled, stroked)

  override def rounded(rx: Int, ry: Int) = RectComplete(dimension, position, Some((rx, ry)), filled, stroked)

}

case class RectRoundedFilledStroked(dimension: (Int, Int),
                                    rounded: Option[(Int, Int)],
                                    filled: Option[String],
                                    stroked: Option[(String, Int)])
  extends RectProperties with NoPosition {

  override val position: Option[(Int, Int)] = None

  override def atPosition(x: Int, y: Int) = RectComplete(dimension, Some((x, y)), rounded, filled, stroked)

}

case class RectComplete(dimension: (Int, Int),
                        position: Option[(Int, Int)],
                        rounded: Option[(Int, Int)],
                        filled: Option[String],
                        stroked: Option[(String, Int)]) extends RectProperties