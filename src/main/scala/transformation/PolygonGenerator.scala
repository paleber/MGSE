package transformation

import abstractSyntax.PolygonProperties

object PolygonGenerator {

  def toXml(polygon: PolygonProperties): String = {


    val filledAttribute = polygon.filled.fold("") { f =>
      s"""
         |    fill="$f"""".stripMargin
    }

    val strokeAttribute = polygon.stroked.fold("") { s =>
      s"""
         |    stroke="${s._1}"
         |    stroke-width="${s._2}"""".stripMargin
    }

    val pointsAttribute =
      s"""
         |    points="${
        polygon.points.map { case (x, y) =>
          s"$x,$y"
        }.mkString(" ")
      }"""".stripMargin

    val rotatedAttribute = polygon.rotated.fold("") { r =>
      s"""
         |    transform="rotate(${r._1} ${r._2} ${r._3})"""".stripMargin
    }

    s"""<polygon $pointsAttribute$filledAttribute$strokeAttribute$rotatedAttribute />""".stripMargin
  }
}
