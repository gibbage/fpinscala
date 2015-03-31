package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  // They don't have to be different types. This works fine too:
  // def foldRight[A](as: List[A], z: A)(f: (A, A) => A): A = // Utility functions
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)

  def sum3(ns: List[Int]) =
    foldRight(ns, "Zero")((x, y) => x + " + " + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("No tail on this donkey")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("No List to set head item on")
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] =  l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = sys.error("todo")

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = sys.error("todo")

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")
}

object ListTesting {
  def main (args: Array[String] ): Unit = {
    println("goodTail: " + List.tail(List(1, 2, 3)))
    //println("badTail: " + List.tail(List()))
    println("goodSetHead: " + List.setHead(List(2, 3, 4), 1))
    //println("badSetHead1: " + List.setHead(List(), 1))
    //println("badSetHead2: " + List.setHead(List(2, 3, 4), Nil))

    println("drop0: " + List.drop(List(1, 2, 3, 4, 5), 0))
    println("drop1: " + List.drop(List(1, 2, 3, 4, 5), 1))
    println("drop2: " + List.drop(List(1, 2, 3, 4, 5), 2))
    println("drop3: " + List.drop(List(1, 2, 3, 4, 5), 3))
    println("drop4: " + List.drop(List(1, 2, 3, 4, 5), 4))
    println("drop5: " + List.drop(List(1, 2, 3, 4, 5), 5))
    println("drop6: " + List.drop(List(1, 2, 3, 4, 5), 6))
    println("drop7: " + List.drop(List(1, 2, 3, 4, 5), 7))
    println("dropWhile0: " + List.dropWhile(List(1, 2, 3), (x: Int) => x != 2))
    println("dropWhile1: " + List.dropWhile(List(1, 2, 3), (x: Int) => x < 3))
    println("dropWhile2: " + List.dropWhile(List(1, 2, 3), (x: Int) => x < 10))

    println("init0: " + List.init(List(1, 2, 3)))
    println("init1: " + List.init(List(1, 2, 3, 4, 5)))
    println("init2: " + List.init(List(1, 2)))

    println("sum0: " + List.sum(List(1,2,3)))
    println("sum1: " + List.sum2(List(1,2,3)))
    println("stringSum0: " + List.sum3(List(1,2,3))) // See what we can do with different type :)
    println("product0: " + List.product(List(1,2,3)))
    println("product1: " + List.product2(List(1,2,3)))

    println("subs0: " + List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))

    // Trace the problem - should all println the same output.
    println("trace0: " + List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
    println("trace0: " + Cons(1, List.foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_, _))))
    println("trace0: " + Cons(1, Cons(2, List.foldRight(Cons(3, Nil), Nil:List[Int])(Cons(_, _)))))
    println("trace0: " + Cons(1, Cons(2, Cons(3, List.foldRight(Nil, Nil:List[Int])(Cons(_, _))))))
    println("trace0: " + Cons(1, Cons(2, Cons(3, Nil))))

    // For practice, lets also trace the previous exercise:
    println("trace1: " + (List.foldRight(List(1, 2, 3), 0)((x, y) => x + y)))
    println("trace1: " + (List.foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)((x, y) => x + y)))
    println("trace1: " + (1 + (List.foldRight(Cons(2, Cons(3, Nil)), 0)((x,y) => x + y))))
    println("trace1: " + (1 + (2 + (List.foldRight(Cons(3, Nil), 0)((x,y) => x + y)))))
    println("trace1: " + (1 + (2 + (3 + (List.foldRight(Nil:List[Int], 0)((x,y) => x + y))))))
    println("trace1: " + (1 + (2 + (3 + (0)))))
    println("trace1: " + (6))
  }
}