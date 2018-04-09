object Start extends App {


  val svg = Svg.
    ofWidth(100)
    .ofHeight(200)
    .withElement(
      Rect(200, 100)
    )


  println(svg.toXml)

}
