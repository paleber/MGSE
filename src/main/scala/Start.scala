object Start extends App {


  val svg = Svg.
    ofWidth(700)
    .ofHeight(400)
    .withElement(
      Line
        .from(180, 370)
        .to(500, 50)
        .stroked("black", 15)
    )
    .withElement(
      Rect(500, 200)
        .stroked("black", 20)
        .filled("white")
        .atPosition(x = 100, y = 100)
    )
    .withElement(
      Line
        .from(0, 200)
        .to(700, 200)
        .stroked("black", 20)
    ).withElement(
      Polygon((585, 0),(525, 25),(585, 50))
  )



  println(svg.toXml)

}
