

import com.sun.management.VMOption.Origin

import scala.annotation.tailrec

/** Реализуйте функции для решения следующих задач.
 * Примечание: Попытайтесь сделать все функции с хвостовой рекурсией, используйте аннотацию для подстверждения.
 * рекурсия будет хвостовой если:
 *   1. рекурсия реализуется в одном направлении
 *   2. вызов рекурсивной функции будет последней операцией перед возвратом
 */
object RecursiveFunctions {

  def length[A](as: List[A]): Int = {
    @tailrec
    def loop(rem: List[A], agg: Int): Int = rem match {
      case Cons(_, tail) => loop(tail, agg + 1)
      case Nil()         => agg
    }

    loop(as, 0)
  }

  /* a) Напишите функцию которая записывает в обратном порядке список:
   *        def reverse[A](list: List[A]): List[A]
   */
  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def loop(list: List[A], agg: List[A]): List[A] = list match {
      case Cons(n, t) => loop(t, Cons(n, agg))
      case Nil() => agg
    }
    loop(list, new Nil[A])
  }


  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testReverse[A](list: List[A]): List[A] = reverse(list)

  /* b) Напишите функцию, которая применяет функцию к каждому значению списка:
   *        def map[A, B](list: List[A])(f: A => B): List[B]
   */
  def Map[A, B](list: List[A])(f: A => B): List[B] = {
    @tailrec
    def loop(origin: List[A], agg: List[B]): List[B] = origin match {
      case Cons(n, t) => loop(t, Cons(f(n), agg))
      case Nil() => reverse(agg)
    }
    loop(list, Nil())
  }


  // используйте функцию из пункта  (b) здесь, не изменяйте сигнатуру
  def testMap[A, B](list: List[A], f: A => B): List[B] = Map(list)(f)

  /* c) Напишите функцию, которая присоединяет один список к другому:
   *        def append[A](l: List[A], r: List[A]): List[A]
   */
  def Append[A](l: List[A], r: List[A]): List[A] = {
    @tailrec
    def loop(left: List[A], right: List[A]): List[A] = left match {
      case Cons(n, t) => loop(t, Cons(n, right))
      case Nil() => right
    }
    loop(reverse(l), r)
  }


  // используйте функцию из пункта  (c) здесь, не изменяйте сигнатуру
  def testAppend[A](l: List[A], r: List[A]): List[A] = Append(l, r)

  /* d) Напишите функцию, которая применяет функцию к каждому значению списка:
   *        def flatMap[A, B](list: List[A])(f: A => List[B]): List[B]
   *
   *    она получает функцию, которая создает новый List[B] для каждого элемента типа A в
   *    списке. Поэтому вы создаете List[List[B]].
   */
  def FlatMap[A, B](list: List[A])(f: A => List[B]): List[B] = {
    @tailrec
    def loop(origin: List[A], result: List[B]): List[B] = origin match {
      case Cons(n, t) => loop(t, Append(f(n), result))
      case Nil() => reverse(result)
    }
    loop(list, new Nil[B])
  }


  // используйте функцию из пункта  (d) здесь, не изменяйте сигнатуру
  def testFlatMap[A, B](list: List[A], f: A => List[B]): List[B] = FlatMap(list)(f)

  /* e) Вопрос: Возможно ли написать функцию с хвостовой рекурсией для `Tree`s? Если нет, почему? */
  /*
  * Если необходимо найти какое-то значение, то можно написать ф-цию с хвостовой рекурсией.
  * Если необходимо проверить оба значения в Node, тогда реализация может стать невыполнимой из-за
  * необходимости запуска двух функций для каждого Node*/

  def main(args: Array[String]): Unit = {
    println("#1")
    println(testReverse(Cons(1, Cons(2, Cons(3, Nil())))))
    println()
    println("#2")
    println(testMap(Cons(1, Cons(2, Cons(3, Nil()))), (x: Int) => x + 2))
    println()
    println("#3")
    println(testAppend(Cons[Int](1, Cons(2, Cons(3, Nil()))), Cons[Int](4, Cons(5, Cons(6, Nil())))))
    println()
    println("#4")
    println(FlatMap(Cons[Int](1, Cons(2, Cons(3, Nil()))))((x: Int) => Cons(x, Nil())))
  }
}
