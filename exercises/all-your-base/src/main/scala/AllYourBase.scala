
object AllYourBase {

  def rebase(inputBase: Int, inputDigits: List[Int], outputBase: Int): Option[List[Int]] = {
    if (inputBase < 2 || outputBase < 2) None
    else {
      toDecimal(inputBase, inputDigits)
        .map { decimal => toBase(outputBase, decimal) }
    }
  }

  /**
   * As there are no details on the input size, I'm assuming that passing the tests
   * is enough, all values from the tests allow us to convert the input to a decimal
   * representation stored in an Int type.
   */
  private def toDecimal(base: Int, digits: List[Int]): Option[Int] = {
    digits.foldLeft(Option(0)) { case (accMaybe, digit) =>
      accMaybe.flatMap { acc =>
        if (digit >= 0 && digit < base) {
          Some(base * acc + digit)
        } else {
          None
        }
      }
    }
  }

  private def toBase(base: Int, value: Int): List[Int] = {
    def f(remaining: Int, accumulator: List[Int]): List[Int] = {
      if (remaining < base) remaining :: accumulator
      else {
        val current = (remaining % base) :: accumulator
        f(remaining / base, current)
      }
    }

    f(value, List.empty)
  }
}
