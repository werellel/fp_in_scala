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


}
