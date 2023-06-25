package abstractmath

trait Monoid[T] extends Semigroup[T]:
  def empty: T

object Monoid:
  def apply[T: Monoid] = summon[Monoid[T]]

extension [T: Monoid](list: List[T])
  def combineAll: T =
    list.foldLeft(Monoid[T].empty)(_ |+| _)

object MonoidInstances:
  import SemigroupInstances.given
  given Monoid[String] with
    def combine(a: String, b: String) = s"$a $b"
    def empty: String = ""

object IntSum:
  given Monoid[Int] with
    def combine(a: Int, b: Int): Int = a + b
    def empty: Int = 0

object IntProduct:
  given Monoid[Int] with
    def combine(a: Int, b: Int): Int = a * b
    def empty: Int = 1

case class ShoppingCart(items: List[String], total: Double)
object ShoppingCart:
  given Monoid[ShoppingCart] with
    def combine(a: ShoppingCart, b: ShoppingCart) = 
      ShoppingCart(a.items ++ b.items, a.total + b.total)
    def empty: ShoppingCart = ShoppingCart(Nil, 0.0)
  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart =
    shoppingCarts.combineAll


object MonoidPlayground extends App:
  import MonoidInstances.given
  import IntProduct.given
  println(Monoid[Int].empty)
  println((1 to 10).toList.combineAll)

  val shoppingCarts = List(
      ShoppingCart(List("iphone", "shoes"), 799),
      ShoppingCart(List("TV"), 20000),
      ShoppingCart(List(), 0)
    )
  println(ShoppingCart.checkout(shoppingCarts))



