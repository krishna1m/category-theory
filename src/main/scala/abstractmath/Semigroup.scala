package abstractmath

trait Semigroup[T]:
  def combine(a: T, b: T): T

object Semigroup:
  def apply[A: Semigroup] = summon[Semigroup[A]]

extension [A: Semigroup](list: List[A])
  def combineAll: A = list.reduceLeft(Semigroup[A].combine)

object SemigroupInstances:
  given Semigroup[Int] with
    def combine(a: Int, b: Int): Int = a + b
  given Semigroup[String] with
    def combine(a: String, b: String) = s"$a;$b"
end SemigroupInstances

// Contrived Example
case class Expense(name: String, price: Int)
object Expense:
  import SemigroupInstances.given
  given Semigroup[Expense] with
    def combine(a: Expense, b: Expense): Expense =
      Expense(Semigroup[String].combine(a.name, b.name), Semigroup[Int].combine(a.price, b.price))


object SemigroupPlayground extends App:
  import SemigroupInstances.given
  println(Semigroup[Int].combine(1, 2))
  println(Semigroup[String].combine("Hello", "Bartosz Milewski!"))

  val expenses = List[Expense](
      Expense("MacBook M2 Air", 3000),
      Expense("Bugatti Keychron", 1000000),
      Expense("Football", 10)
    )
  println(expenses.combineAll)
end SemigroupPlayground
