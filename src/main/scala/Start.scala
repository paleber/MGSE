object Start extends App {


  val svg = Svg.
    ofWidth(600)

    .withElement(
      Rect(200, 100)
        .rounded(20, 10)
        .stroked("blue", 3)
        .filled("red")
        .atPosition(x = 20, y = 20)
    )
    .ofHeight(800)


  println(svg.toXml)

}
