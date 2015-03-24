sealed trait MyList[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends MyList[Nothing]

// A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

// Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.
object MyList {
  def apply[A](as: A*): MyList[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

val myList = MyList("a", "b", "c")
val anotherList = MyList(1, 4, 2)
val emptyList = MyList()

import fpinscala.datastructures._

val goodTail = List.tail(List(1, 2, 3))
//val badTail = List.tail(List())
val goodSetHead = List.setHead(List(2, 3, 4), 1)
//val badSetHead1 = List.setHead(List(), 1)
//val badSetHead2 = List.setHead(List(2, 3, 4), Nil)
val drop0 = List.drop(List(1, 2, 3, 4, 5), 0)
val drop1 = List.drop(List(1, 2, 3, 4, 5), 1)
val drop2 = List.drop(List(1, 2, 3, 4, 5), 2)
val drop3 = List.drop(List(1, 2, 3, 4, 5), 3)
val drop4 = List.drop(List(1, 2, 3, 4, 5), 4)
val drop5 = List.drop(List(1, 2, 3, 4, 5), 5)
val drop6 = List.drop(List(1, 2, 3, 4, 5), 6)
val drop7 = List.drop(List(1, 2, 3, 4, 5), 7)
val dropWhile0 = List.dropWhile(List(1, 2, 3), (x: Int) => x != 2)
val dropWhile1 = List.dropWhile(List(1, 2, 3), (x: Int) => x < 3)
val dropWhile2 = List.dropWhile(List(1, 2, 3), (x: Int) => x < 10)

val init0 = List.init(List(1, 2, 3))
val init1 = List.init(List(1, 2, 3, 4, 5))
val init2 = List.init(List(1, 2))