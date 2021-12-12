
import scala.annotation.tailrec

/** Напишите свои решения в тестовых функциях.
 *
 * Seq(1, 2) match {
 *   case head +: tail => ???
 *   case Nil          => ???
 *   case s            => ???
 * }
 * Оператор :: создает новый список из заданных значений головы и хвоста списка
 * «разбирает» список на голову и хвост
 * https://www.scala-lang.org/api/2.12.0/scala/collection/Seq.html
 */
// Примечание: напишите функции с хвостовой рекурсией

object Sequence {

  /* a) Найдите последний элемент Seq.
   *
   */
  def testLastElement[A](seq: Seq[A]): Option[A] = {
    @tailrec
    def loop(myseq: Seq[A]): Option[A] = {
      myseq match {
        case Nil => None
        case Seq(a) => Some(a)
        case _::tail => loop(tail)
      }
    }
    loop(seq)
  }

  /* b) Объедините две Seqs (то есть Seq(1, 2) и Seq(3, 4) образуют Seq((1, 3), (2, 4))) - если Seq длиннее игнорируйте оставшиеся элементы.
   *
   */
  def testZip[A](a: Seq[A], b: Seq[A]): Seq[(A, A)] = {
    @tailrec
    def loop(a1: Seq[A], b1: Seq[A], res: Seq[(A, A)]): Seq[(A, A)] = {
      a1 match {
        case head1 :: Nil => b1 match {
          case Nil => res
          case head2 :: _ => res:+(head1, head2)
        }
        case head1 :: tail1 => b1 match {
          case Nil => res
          case head2 :: tail2 => loop(tail1, tail2, res:+(head1, head2))
        }
        case Nil => res
      }
    }
    loop(a, b, Nil)
  }

  /* c) Проверьте, выполняется ли условие для всех элементов в Seq.
   * def func1(a: Int): Boolean = a < 10
   */
  def testForAll[A](seq: Seq[A])(cond: A => Boolean): Boolean = {
    @tailrec
    def loop(seq: Seq[A], res: Boolean): Boolean = {
      seq match {
        case head::tail => loop(tail, res && cond(head))
        case Nil => res
      }
    }
    loop(seq, true)
  }

  /* d) Проверьте, является ли Seq палиндромом
   *
   */
  def testPalindrom[A](seq: Seq[A]): Boolean = {
    @tailrec
    def loop(seq: Seq[A], len: Int): Int = {
      seq.length match {
        case 0 => len
        case 1 => len
        case _ => if (seq.head == seq.last) loop(seq.tail.init, len + 1) else len
      }
    }
    loop(seq, 0) == seq.length/2
  }

  /* e) Реализуйте flatMap используя foldLeft.
   *
   */
  def testFlatMap[A, B](seq: Seq[A])(f: A => Seq[B]): Seq[B] = seq.foldLeft(Seq[B]())((x,y) => x++:f(y))

  def main(args: Array[String]): Unit = {
    var seq1 = Seq(1, 2, 3, 2, 1)
    var seq2 = Seq(1, 7)
    println("#1")
    println(testLastElement(seq1))
    println()
    println("#2")
    println(testZip(seq1, seq2))
    println()
    println("#3")
    def func1(a: Int): Boolean = a < 10
    println(testForAll(Seq(1, 5, 9))(func1))
    println(testForAll(Seq(1, 11, 9))(func1))
    println()
    println("#4")
    println(testPalindrom(seq1))
    println()
    println("#5")
    def func(in: String): Seq[Char] = in.toUpperCase.toSeq
    println(testFlatMap(Seq("one", "two"))(func))
  }

}
