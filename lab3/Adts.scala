
import scala.util.{Try, Failure, Success}

/** Реализуйте следующие функции.
 *
 * List(1, 2) match {
 *   case head :: tail => ???
 *   case Nil          => ???
 *   case l            => ???
 * }
 *
 * Option(1) match {
 *   case Some(a) => ???
 *   case None    => ???
 * }
 *
 * Either.cond(true, 1, "right") match {
 *   case Left(i)  => ???
 *   case Right(s) => ???
 * }
 *
 * Try(impureExpression()) match {
 *   case Success(a)     => ???
 *   case Failure(error) => ???
 * }
 *
 * Try(impureExpression()).toEither
 *
 */
object Adts {

  // a) Дан List[Int], верните элемент с индексом n
  def getNth(list: List[Int], n: Int): Option[Int] = {
    list match {
      case head :: tail => Some(list(n))
      case Nil => None
      case null => None
    }
  }

  // примените функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testGetNth(list: List[Int], n: Int): Option[Int] = getNth(list, n)

  // b) Напишите функцию, увеличивающую число в два раза.
  def double(n: Option[Int]): Option[Int] = {
    n match {
      case Some(a) => Some(2*a)
      case None => None
    }
  }


  // примените функцию из пункта (b) здесь, не изменяйте сигнатуру
  def testDouble(n: Option[Int]): Option[Int] = double(n)

  // c) Напишите функцию, проверяющую является ли число типа Int четным. Если так, верните Right. В противном случае, верните Left("Нечетное число.").
  def IsEven(n: Int): Either[String, Int] = {
    Either.cond(n % 2 == 0, n, "Нечетное число") match {
      case Left(i) => Left(i)
      case Right(s) => Right(s)
    }
  }
  // Either - тип общего назначения, в котором возмодны 2 альтернативы: успешно/неуспешно


  // примените функцию из пункта (c) здесь, не изменяйте сигнатуру
  def testIsEven(n: Int): Either[String, Int] = IsEven(n)

  // d) Напишите функцию, реализующую безопасное деление целых чисел. Верните Right с результатом или Left("Вы не можете делить на ноль.").
  def safeDivide(a: Int, b: Int): Either[String, Int] = {
    Either.cond(b != 0, a/b, "Вы не можете делить на ноль") match {
      case Left(i) => Left(i)
      case Right(s) => Right(s)
    }
  }


  // примените функцию из пункта (d) здесь, не изменяйте сигнатуру
  def testSafeDivide(a: Int, b: Int): Either[String, Int] = safeDivide(a, b)

  // e) Обработайте исключения функции с побочным эффектом вернув 0.
  def goodOldJava(a: String => Int, str: String): Try[Int] = {
    Try(a(str)) match {
      case Success(a) => Success(a)
      case Failure(error) => Failure(error)
    }
  }
    //все, что не читает аргументы и не возвращает результат

  // примените функцию из пункта (e) здесь, не изменяйте сигнатуру
  def testGoodOldJava(impure: String => Int, str: String): Try[Int] = goodOldJava(impure, str)

  def main(args: Array[String]): Unit = {
    println("#1")
    println(testGetNth(List(1,2,3,4,5),4))
    println()
    println("#2")
    println(testDouble(Some(2)))
    println()
    println("#3")
    println(testIsEven(5))
    println()
    println("#4")
    println(testSafeDivide(10, 2))
    println()
    println("#5")
    def func1(str: String): Int = str(0).toInt/0
    def func2(str: String): Int = str.toInt
    println(testGoodOldJava(func1, "123"))
    println(testGoodOldJava(func2, "123"))

  }
}
