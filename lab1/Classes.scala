package exercise1

/*
 *
 a) Создать класс Animal, который имеет следующие поля:
 *      - name: String (название)
 *      - species: String (вид)
 *      - food: String
 *
 *    Синтаксис: class MyClass(val publicField: Int, privateField: String) {
 *              // остальные поля и методы
 *            }
 *
 *
 *
 *
 * c) Добавьте следующие метод в Animals:
 *      def eats(food: String): Boolean
 *
 *     который проверяет ест ли животное определенную пищу
 *
 *
 *
 *


 */

sealed trait Option[A] {
  def isEmpty: Boolean
}

case class Some[A](a: A) extends Option[A] {
  val isEmpty = false
}

case class None[A]() extends Option[A] {
  val isEmpty = true
}

// class Animal(val name: String, val species: String, val food: Food) {
//   def eats(food: String) {
//     food == this.food
//   }
// }

//b) Создайте объект-компаньон для класса Animal и добавьте следующие сущности как поля:
//  *      - cat, mammal, meat
//  *      - parrot, bird, vegetables
//  *      - goldfish, fish, plants
//  *
//  *    Синтаксис: object MyClass {
//    *              // статические поля и методы
//    *            }
//  *
object Animal {
  val cat = new Mammal("cat", Meat, 3)
  val parrot = new Bird("parrot", Vegetables)
  val goldfish = new Fish("goldfish", Plants)

//  * e) Добавьте следующие функции в объект-компаньон Animal:
//    *      def knownAnimal(name: String): Boolean  // true если это имя одного из трех животных из (b)
//  *      def apply(name: String): Option[Animal] // возвращает одно из трех животных в соответствии с именем (Some) или ничего (None), см. ниже
//  *
  def knownAnimal(name: String) = {
    name == cat.name || name == parrot.name || name == goldfish.name
  }

  def main(args: Array[String]):Unit ={
    println(Animal.knownAnimal("bird"))
    println(Animal.apply("cat"))
    println(Food.apply("meat"))
  }

  def apply(name: String): Option[Animal] = name match {
    case "cat" => Some(cat)
    case "parrot" => Some(parrot)
    case "goldfish" => Some(goldfish)
    case _ => None()
  }
}
//d) Переопределите ваш класс Animal как трейт и создайте объекты класса-образца для Mammals, Birds и Fishs.
//*    Вам все еще нужно поле `species`?
trait Animal {
  val name: String
  val food: Food
}

case class Mammal(name: String, food: Food, weight: Int) extends Animal

case class Bird(name: String, food: Food) extends Animal

case class Fish(name: String, food: Food) extends Animal

//* f) Создайте трейт Food со следующими классами-образцами:
//  *      - Meat
//  *      - Vegetables
//  *      - Plants
//  *   и добавьте это в определение Animal. Так же добавьте объект-компаньон с методом apply():
//  *      def apply(food: String): Option[Food]
trait Food

case object Meat extends Food

case object Vegetables extends Food

case object Plants extends Food

object Food {
  def apply(name: String): Option[Food] = name match {
    case "meat" => Some(Meat)
    case "vegetable" => Some(Vegetables)
    case "plants" => Some(Plants)
    case _ => None()
  }
}
