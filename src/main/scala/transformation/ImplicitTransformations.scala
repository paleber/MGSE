package transformation

import dsl.abstractSyntax.LineProperties

object ImplicitTransformations {

  implicit class Line(line: LineProperties) {
    def toXml: String = LineGenerator.toXml(line)
  }

}
