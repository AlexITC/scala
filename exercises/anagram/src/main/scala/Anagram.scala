
object Anagram {
  def anagrams(word: String, anagrams: Seq[String]): Seq[String] = {
    // we need case insensitive matches
    val lowerCaseWord = word.toLowerCase
    val sortedWord = lowerCaseWord.sorted
    anagrams.filter { candidate =>
      val lowerCaseCandidate = candidate.toLowerCase
      lowerCaseCandidate != lowerCaseWord &&
        lowerCaseCandidate.sorted == sortedWord
    }
  }
}
