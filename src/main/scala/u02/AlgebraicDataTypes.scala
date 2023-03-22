package u02

import u03.Lists.*

object AlgebraicDataTypes extends App :

  import List.*

  enum Person: // a sum type defined by enumerating various cases
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  def name(p: Person): String = p match
    case Person.Student(n, _) => n
    case Person.Teacher(n, _) => n

  def isStudent(p: Person): Boolean = p match
    case Person.Student(_, _) => true
    case _ => false

  def courses(l: List[Person]): List[String] = l match
    case Cons(h, t) => map(filter(l)(!isStudent(_)))(p => p match
      case Person.Teacher(_, course) => course)
    case _ => Nil()

  println(name(Person.Student("mario", 2015)))

  enum WeekDay: // a Java-like enumeration
    case Monday, Tuesday, Wednesday, Thursday, Friday

  def preferredDay(d: WeekDay): Boolean =
    d == WeekDay.Friday // could have used matching instead

  // A LinkedList of Int
  enum IntList: // a recursive type
    case Cons(head: Int, tail: IntList)
    case Nil

  def sum(l: IntList): Int = l match
    case IntList.Cons(h, t) => h + sum(t)
    case _ => 0

  println(sum(IntList.Cons(10, IntList.Cons(20, IntList.Nil))))