

object Typeclasses {

  // a) Определите тайп-класс Reversable, который представляет в обратном порядке значения.
  trait Reversable[T] {def reverse(value: T): T}

  // b) Реализуйте функцию Reverse для String.
  object Reversable {
    def reverse[T: Reversable](a: T): T = implicitly[Reversable[T]].reverse(a)
    implicit object ReversableString extends Reversable[String] {
      def reverse(str: String): String = str.reverse
    }
  }

  // примените тайп-класс-решение из пункта (a) здесь
  def testReversableString(str: String): String = Reversable.reverse(str)

  // c) Определите тайп-класс Smash таким образом, чтобы в нем была функция smash, которая выполняет операцию со значениями одного типа.

  trait Smash[T]{def smash(a: T, b: T): T}

  object Smash {
    def smash[T : Smash](a: T, b: T): T = implicitly[Smash[T]].smash(a, b)
    implicit object SmashInt extends  Smash[Int] {
      def smash(val1: Int, val2: Int): Int = val1 + val2
    }
    implicit object SmashDouble extends  Smash[Double] {
      def smash(val1: Double, val2: Double): Double = val1 * val2
    }
    implicit object SmashString extends  Smash[String] {
      def smash(val1: String, val2: String): String = val1 + val2
    }
  }

  // d) Реализуйте  функции Smash для типа Int и Double.
  //    Используйте сложение для типа Int у умножение для типа Double.



  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashInt(a: Int, b: Int): Int = Smash.smash(a, b)

  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashDouble(a: Double, b: Double): Double = Smash.smash(a, b)


  // e) Реализуйте функцию Smash для типа String. Необходимо выполнить конкатенацию строк, которые будут получены в качестве параметра.



  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashString(a: String, b: String): String = Smash.smash(a, b)

  def main(args: Array[String]): Unit = {
    println("#1")
    println(testReversableString("Hello"))
    println()
    println("#2")
    println(testSmashInt(2, 3))
    println()
    println("#3")
    println(testSmashDouble(2.3, 0.5))
    println()
    println("#4")
    println(testSmashString("Hello, ", "world"))
  }
}

// Реализуйте тестовые функции с выводом на экран проверки разработанных функций.

