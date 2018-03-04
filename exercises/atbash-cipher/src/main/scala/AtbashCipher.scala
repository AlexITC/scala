object AtbashCipher {

  private lazy val Charset = ('a' to 'z').toList
  private lazy val ReversedCharset = Charset.reverse

  private lazy val EncodeMap = Charset
      .zipWithIndex
      .map { case (char, index) => char -> ReversedCharset(index) }
      .toMap

  private lazy val DecodeMap = ReversedCharset
      .zipWithIndex
      .map { case (char, index) => char -> Charset(index) }
      .toMap

  def encode(s: String): String = {
    encodeWith(s, EncodeMap).grouped(5).mkString(" ")
  }

  def decode(s: String): String = {
    encodeWith(s, DecodeMap)
  }

  private def encodeWith(s: String, mapping: Map[Char, Char]) = {
    s.toLowerCase.flatMap { char =>
      if (char.isDigit) Some(char)
      else mapping.get(char)
    }
  }
}
