package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*
import u02.Optionals.*
import u02.AlgebraicDataTypes.*

class ListTest:
  import List.*
  import Option.{Some, None}
  import Person.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))
  val p: List[Person] = Cons(Person.Teacher("Mario", "Geografia"), Cons(Person.Teacher("Giovanni", "Scienze"),
    Cons(Person.Student("Francesco", 2014), Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_+1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_+""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_>=20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_!=20))

  @Test def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l)(1))
    assertEquals(Cons(30, Nil()), drop(l)(2))
    assertEquals(Nil(), drop(l)(5))

  @Test def testAppend() =
    assertEquals(Cons(40,Cons(10, Cons(20, Cons(30, Nil())))), append(Cons(40, Nil()))(l))
    assertEquals(Cons(40, Cons(50, Cons(10, Cons(20, Cons(30, Nil()))))), append(Cons(40, Cons(50, Nil())))(l))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil ())))))),
      flatMap(l)(v => Cons( v + 1, Cons(v + 2, Nil () ))))

  @Test def testMax() =
    assertEquals(Some(30), max(l))
    assertEquals(None(), max(Nil()))

  @Test def testCourses() =
    assertEquals(Cons("Geografia", Cons("Scienze", Nil())), courses(p))

  @Test def testFoldLeft() =
    assertEquals(60, foldLeft(l)(0)(_ + _))
    assertEquals(-55, foldLeft(l)(5)(_ - _))

  @Test def testReduce() =
    assertEquals(0, reduce(0))
    assertEquals(3, reduce(3))

  @Test def testFoldRight() =
    assertEquals(20, foldRight(l)(0)(_ - _))
    assertEquals(12000, foldRight(l)(2)(_ * _))

