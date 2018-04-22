package transformation

import abstractSyntax.LineProperties

object LineGenerator {

  def toXml(line: LineProperties): String = {
    val fromAttribute = {
      s"""
         |    x1="${line.from._1}"
         |    y1="${line.from._2}"""".stripMargin
    }

    val toAttribute = {
      s"""
         |    x2="${line.to._1}"
         |    y2="${line.to._2}"""".stripMargin
    }

    val strokeAttribute = line.stroked.fold("") { s =>
      s"""
         |    stroke="${s._1}"
         |    stroke-width="${s._2}"""".stripMargin
    }

    s"""<line $fromAttribute$toAttribute$strokeAttribute />""".stripMargin
  }

}
