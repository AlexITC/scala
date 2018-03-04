
object Alphametics {

  def solve(input: String): Option[Map[Char, Int]] = {
    Expression
      .from(input)
      .flatMap { expression =>
        val charset = expression.charset.toList ++ Seq.fill(10 - expression.charset.size)('?')
        charset
            .permutations
            .toStream
            .filter { perm => isSolution(expression, perm.zipWithIndex.toMap) }
            .take(1)
            .headOption
            .map(_.zipWithIndex.toMap)
            .map { values =>
              val f = expression.charset.contains _
              values.filterKeys(f)
            }
      }
  }

  private def isSolution(expression: Expression, values: Map[Char, Int]): Boolean = {
    val result = for {
      left <- {
        val list = expression.operations.flatMap { term => toInt(term, values) }
        if (list.lengthCompare(expression.operations.size) == 0) Some(list.sum)
        else None
      }
      right <- toInt(expression.result, values)
    } yield left == right

    result.exists(identity)
  }

  // map a string to its integer representation, fails it it has leading zeroes
  private def toInt(string: String, values: Map[Char, Int]): Option[Int] = {
    val result = string
      .map { char => values(char) }
      .foldLeft(0) { case (acc, value) =>
        acc * 10 + value
      }

    Option(result).filter { _.toString.length == string.length }
  }
}


case class Expression(operations: List[String], result: String) {
  lazy val charset = (result :: operations).mkString("").toSet
}

object Expression {

  /**
   * Based on the tests, the expected format is "A == B" or "A + B == C", being
   * able to use the '+' operator several times
   */
  def from(string: String): Option[Expression] = {
    val expressionMaybe = for {
      Array(op, r) <- Option { string.split("==") }.filter(_.length == 2)
      operations <- Option { op.split(raw"\+").map(_.trim).filter(_.nonEmpty) }.filter(_.nonEmpty)
      result <- Option(r).map(_.trim).filter(_.nonEmpty)
    } yield Expression(operations.toList, result)

    // restrict to characters from A to Z only with up-to 10 unique characters
    expressionMaybe.filter { expression =>
      expression.charset.size <= 10 &&
        expression.charset.forall { char => char >= 'A' && char <= 'Z' }
    }
  }
}
