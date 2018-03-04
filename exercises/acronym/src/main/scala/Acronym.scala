object Acronym {
  def abbreviate(phrase: String): String = {
    phrase
      .split(raw"\W")
      .flatMap(_.headOption)
      .mkString("")
      .toUpperCase
  }
}
