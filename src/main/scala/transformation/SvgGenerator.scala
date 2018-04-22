package transformation

import java.io.{File, PrintWriter}

import dsl.abstractSyntax.{LineProperties, PolygonProperties, RectProperties, SvgProperties}


object SvgGenerator {

  def toXml(svg: SvgProperties): String = {

    val widthAttribute = svg.width.fold("") { w =>
      s"""
         |  width="$w"""".stripMargin
    }

    val heightAttribute = svg.height.fold("") { h =>
      s"""
         |  height="$h"""".stripMargin
    }

    val children = svg.elements.map(x =>
      s"""
         |  ${
        x match {
          case r: RectProperties => RectGenerator.toXml(r)
          case l: LineProperties => LineGenerator.toXml(l)
          case p: PolygonProperties => PolygonGenerator.toXml(p)
        }
      }
      """.stripMargin)

    s"""<svg xmlns="http://www.w3.org/2000/svg"$widthAttribute$heightAttribute>
       |${children.mkString}
       |</svg>
    """.stripMargin

  }

  def generateXml(svg: SvgProperties): Unit = {
    val writer = new PrintWriter(new File("src\\main\\scala\\xml\\s.svg"))
    writer.write(SvgGenerator.toXml(svg))
    writer.close()
  }
}
