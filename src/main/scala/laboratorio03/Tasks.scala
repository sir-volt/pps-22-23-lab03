package laboratorio03

import u02.Optionals
import u02.Optionals.Option.*
import u02.AlgebraicDataTypes.*

object Tasks extends App:

  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  // a companion object (i.e., module) for List
  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    //TASK 1.d: map and filter method changed to use flatMap
    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(_, _) => flatMap(l)(h => Cons(mapper(h), Nil()))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(_, _) => flatMap(l1)(h => h match
        case h if (pred(h)) => Cons(h, Nil())
        case _ => Nil())
      case _ => Nil()

    //TASK 1.a: Drop method
    def drop[A](l1: List[A])(dropper: Int): List[A] = l1 match
      case Cons(_, t) if dropper > 1 => drop(t)(dropper - 1)
      case Cons(_, t) => t
      case _ => Nil()

    //TASK 1.b: Append method
    def append[A](left: List[A])(right: List[A]): List[A] = left match
      case Cons(h, t) => Cons(h, append(t)(right))
      case Nil() => right

    //TASK 1.c: FlatMap method
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h))(flatMap(t)(f))
      case Nil() => Nil()

    //TASK 2: Max method
    def max(l: List[Int]): Optionals.Option[Int] = l match
      case Cons(h, t) if (h > orElse(max(t), 0)) => Some(h)
      case Cons(_, t) => max(t)
      case Nil() => None()

    //TASK 3: implementing a method that returns List[String] containing each courses from a
    //List[Person] (we are using enum Person and method isStudent from AlgebraicDataTypes for this)
    def courses(l: List[Person]): List[String] = l match
      case Cons(h, t) => map(filter(l)(!isStudent(_)))(p => p match
        case Person.Teacher(_, course) => course)
      case _ => Nil()

    //TASK 4: FoldLeft, FoldRight and Reduce methods
    def foldLeft[A, B](l: List[A])(start: B)(f: (B, A) => B): B = l match
      case Cons(h, t) => foldLeft(t)(f(start, h))(f)
      case Nil() => start

    def reduce[A](start: A): A = foldLeft(Nil())(start)((start, _) => start)

    def foldRight[A, B](l: List[A])(start: B)(f: (A, B) => B): B = l match
      case Cons(h, t) => f(h, foldRight(t)(start)(f))
      case Nil() => start

  end List

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    //TASK 6: generic function "constant" for streams
    def constant[A](value: => A): Stream[A] =
      lazy val const = value
      Cons(() => const, () => constant(value))

    //TASK 7: Fibonacci recreated using Streams (does not work)
    def fibonacci(previousNum: Int)(start: Int): Stream[Int] = (previousNum, start) match
      case (_, 0) => cons(0, fibonacci(0)(1))
      case (0, 1) => cons(1, fibonacci(1)(1))
      case (n, m) => cons(n + m, fibonacci(m)(n + m))

    def toList[A](stream: Stream[A]): List[A] = stream match
      case Cons(h, t) => List.Cons(h(), toList(t()))
      case _ => List.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    //TASK 5: Drop method for Streams
    def drop[A](stream: Stream[A])(n: Int): Stream[A] = stream match
      case Cons(_, tail) if n > 1 => drop(tail())(n - 1)
      case Cons(_, tail) => tail()
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

  end Stream

