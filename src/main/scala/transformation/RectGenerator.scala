package transformation

import abstractSyntax.RectProperties

object RectGenerator {

  def toXml(rect: RectProperties): String = {

    val positionAttribute = rect.position.fold("") { p =>
      s"""
         |    x="${p._1}"
         |    y="${p._2}"""".stripMargin
    }

    val roundedAttribute = rect.rounded.fold("") { r =>
      s"""
         |    rx="${r._1}"
         |    ry="${r._2}"""".stripMargin
    }

    val filledAttribute = rect.filled.fold("") { f =>
      s"""
         |    fill="$f"""".stripMargin
    }

    val strokeAttribute = rect.stroked.fold("") { s =>
      s"""
         |    stroke="${s._1}"
         |    stroke-width="${s._2}"""".stripMargin
    }

    s"""<rect
       |    width="${rect.dimension._1}"
       |    height="${rect.dimension._2}"$positionAttribute$roundedAttribute$filledAttribute$strokeAttribute />""".stripMargin
  }
}
