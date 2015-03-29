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
