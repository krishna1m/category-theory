package abstractmath

trait Semigroup[T]:
  def combine(a: T, b: T): T

object Semigroup:
  def apply[T: Semigroup] = summon[Semigroup[T]]

extension [T: Semigroup](a: T)
  def |+|(b: T): T = Semigroup[T].combine(a, b)

extension [A: Semigroup](list: List[A])
  def reduceAll: A = list.reduceLeft(_ |+| _)

object SemigroupInstances:
  // problem: could not define Semigroup[Option[T]]
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
  println(1 |+| 2)
  println(Semigroup[String].combine("Hello", "Bartosz Milewski!"))
  println("Hello" |+| "Bartosz Milewski!")

  val expenses = List[Expense](
      Expense("MacBook M2 Air", 3000),
      Expense("Bugatti Keychron", 1000000),
      Expense("Football", 10)
    )
  println(expenses.reduceAll)
end SemigroupPlayground
