
case class Binary(s: String) {
  val toDecimal: Int = {
    val resultMaybe = s.foldLeft(Option(0)) { case (acc, bit) =>
      bit match {
        case '0' | '1' => acc.map(_ * 2 + bit - '0')
        case _ => None
      }
    }

    resultMaybe.getOrElse(0)
  }
}
