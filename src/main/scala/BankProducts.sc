trait BankProducts {
  val name: String
  var balance: Double

  def addMoney(money: Double) = {
    balance = balance + money
  }
  def withdraw(money: Double): Boolean = {
    if (balance >= money) {
      balance = balance - money
      true
    }
    else
      false
  }
}

object BankProductType extends Enumeration {
  val CreditCard, DebetCard, Debet = Value
}

object BankProducts {
  def apply(kind: BankProductType.Value, initBalance: Double) = kind match {
    case BankProductType.CreditCard => new CreditCard(initBalance)
    case BankProductType.DebetCard => new DebetCard(initBalance)
    case BankProductType.Debet => new Debet(initBalance)
  }
}

class CreditCard (override var balance: Double) extends BankProducts{
  val name:String = "кредитная карта"
  var creditLimit:Double = 0.0

  def changeCreditLimit(newLimit:Double) : Unit = {
    if (newLimit > 0)
      creditLimit = newLimit
  }

  override def withdraw(money: Double): Boolean = {
    if (balance + creditLimit >= money) {
      balance = balance - money
      true
    }
    else
      false
  }
}

class DebetCard (override var balance: Double) extends BankProducts{
  val name:String = "дебетовая карта"
}

class Debet (override var balance: Double) extends BankProducts{
  val name:String = "вклад"
}

val debet = BankProducts(BankProductType.Debet, 1000)
debet.addMoney(500)
val debetCard = BankProducts(BankProductType.DebetCard, 1234)
debetCard.withdraw(700)
val creditCard = BankProducts(BankProductType.CreditCard, 4000)
creditCard.asInstanceOf[CreditCard].changeCreditLimit(2000)
creditCard.withdraw(5000)

val products: List[BankProducts] = List(debet, creditCard, debetCard)

products.map(_.name)
val sumBalance = products.map(_.balance).sum
