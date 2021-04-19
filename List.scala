package ch03

// 형식 A에 대해 매개변수화된 List 자료 형식.
sealed trait List[+A]

// 빈 목록을 나타내는 List 자료 생성자.
case object Nil extends List[Nothing]

// 비지 않은 목록을 나타내는 또 다른 자료 생성자.
// 또 다른 List[A]로 Nil일 수도 있고 다른 Cons일 수도 있다.
case class Cons[+A](head:A, tail: List[A]) extends List[A]

// List 동반 객체. 목록의 생성과 조작을 위한 함수들을 담는다.
object List {
  // 패턴 부합으로 목록의 정수들을 합하는 함수
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    // x로 시작하는 목록의 합은 x + 목록의 나머지 부분의 합
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  // 가변 인수 함수 구문
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }
  // f의 인수 형식을 스칼라가 추론할 수 있도록 f를 as, z 다음의 개별적인 인수 그룹에 넣었다.
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)
  // _ * _dms (x, y) => x * y를 좀 더 간결하게 표기한 것
  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_,acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc,h) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h,t) => Cons(h.toString,t))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))

  def map_1[A,B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(l, Nil:List[B])((h,t) => Cons(f(h),t))

  def map_2[A,B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => buf += f(h); go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)

  def filter_1[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(l, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)

  def filter_2[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => if (f(h)) buf += h; go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
  }


  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Cons(h,t) if f(h) => dropWhile(t)(f)
      case _ => as
    }

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }
  def main(args: Array[String]) {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => println(x)
      case Nil => println(42)
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => println(x + y + 1000)
      case Cons(h, t) => println(h + sum(t))
      case _ => println(101)
    }
    println {
      foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    }
    println(length(List(1, 2, 3)))

    val xs = List(1, 10, 2, 4, 5)
    val ex1 = dropWhile(xs)(x => x < 4)
    println(ex1)
    println(length(xs))
    val xt = List(List(1, 5), List(1, 4, 5), List(1, 10, 2, 4, 5))
    val fm2 = filterViaFlatMap(xt)(x => length(x) > 1)
    println("fm2: ", fm2)
  }
}
