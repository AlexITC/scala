
object Allergies {

  import Allergen._

  private lazy val list = List(
    Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats
  )

  /**
   * As the scores for an allergen contain just a single bit turned on,
   * it is quite simple to use the binary and operator to test for an
   * allergen.
   */
  def allergicTo(allergen: Allergen, score: Int): Boolean = {
    (allergen.score & score) != 0
  }

  def list(score: Int): List[Allergen] = {
    list.filter { allergen =>
      allergicTo(allergen, score)
    }
  }
}

sealed abstract class Allergen(val score: Int)
object Allergen {
  case object Eggs extends Allergen(1)
  case object Peanuts extends Allergen(2)
  case object Shellfish extends Allergen(4)
  case object Strawberries extends Allergen(8)
  case object Tomatoes extends Allergen(16)
  case object Chocolate extends Allergen(32)
  case object Pollen extends Allergen(64)
  case object Cats extends Allergen(128)
}
