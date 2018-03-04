trait BankAccount {

  def closeAccount(): Unit

  def getBalance: Option[Int]

  def incrementBalance(increment: Int): Option[Int]
}

object Bank {
  def openAccount(): BankAccount = {
    new CustomBankAccount
  }
}

class CustomBankAccount extends BankAccount {

  private val lock = new Object

  private var balance = 0
  private var isAvailable = true

  override def closeAccount(): Unit = withLock {
    isAvailable = false
  }

  override def getBalance: Option[Int] = withLock {
    Option(isAvailable)
      .filter(identity)
      .map(_ => balance)
  }

  override def incrementBalance(increment: Int): Option[Int] = withLock {
    val result = Option(isAvailable)
      .filter(identity)
      .map { _ =>
        balance + increment
      }

    result.foreach { newBalance => balance = newBalance }
    result
  }

  private def withLock[A](f: => A) = lock.synchronized(f)
}
