

/** Напишите вашу реализацию в тестовые функции.
 *
 * https://docs.scala-lang.org/overviews/collections/maps.html
 */
object Maps {

  case class User(name: String, age: Int)

  /* a) В данной Seq[User] сгруппируйте пользователей по имени (`groupBy`) и вычислите средний возраст: `name -> averageAge`
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testGroupUsers(users: Seq[User]): Map[String, Int] = {
    def srage(num: Seq[Int]) = num.sum/num.length

    users.groupBy(_.name).map{
      myKey => (myKey._1, srage(myKey._2.map(_.age)))
    }
  }

  /* b) Дана `Map[String, User]` состоящая из имен пользователей `User`, сколько имен пользователей, содержащихся в Map, содержат подстроку "Adam"?
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testNumberFrodos(map: Map[String, User]): Int = {
    map.count(x => x._2.name.contains("Adam"))
  }

  /* c) Удалите всех пользователей возраст которых менее 35 лет.
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testUnderaged(map: Map[String, User]): Map[String, User] = {
    map.filter(x => x._2.age >= 35)
  }

  def main(args: Array[String]) = {
    println("#1")
    println(testGroupUsers(Seq(User("Anna", 20), User("Anna", 56), User("Liza", 23))))
    println()
    println("#2")
    var map = Map("One" -> User("Adam", 23), "Two" -> User("Adam", 15), "Three" -> User("Lena", 44))
    println(testNumberFrodos(map))
    println()
    println("#3")
    println(testUnderaged(map))
  }
}
