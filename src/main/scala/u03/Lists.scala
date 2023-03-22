package u03

import u02.Optionals
import u02.Optionals.Option.*

object Lists extends App :

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:
    
    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(_, _) => flatMap(l)(h => Cons(mapper(h), Nil()))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(_, _) => flatMap(l1)(h => h match
        case h if(pred(h)) => Cons(h, Nil())
        case _ => Nil())
      case _ => Nil()

    def drop[A](l1: List[A])(dropper: Int): List[A] = l1 match
      case Cons(_, t) if dropper > 1 => drop(t)(dropper - 1)
      case Cons(_, t) => t
      case _ => Nil()

    def append[A](left: List[A])(right: List[A]): List[A] = left match
      case Cons(h, t) => Cons(h, append(t)(right))
      case Nil() => right

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h))(flatMap(t)(f))
      case Nil() => Nil()

    def max(l: List[Int]): Optionals.Option[Int]  = l match
      case Cons(h, t) if (h > orElse(max(t), 0)) => Some(h)
      case Cons(_, t) => max(t)
      case Nil() => None()

  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
