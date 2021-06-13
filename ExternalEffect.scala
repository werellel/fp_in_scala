package ch13

import ch11.Monad

import scala.io.StdIn.readLine

case class Player(name: String, score: Int)


object main {

//  def contest(p1: Player, p2: Player): Unit =
//    if (p1.score > p2.score)
//      println(s"${p1.name} is the winner!")
//    else if (p2.score > p1.score)
//      println(s"${p2.name} is the winner!")
//    else
//      println("It's a draw.")

  // Option: 값이 있거나 또는 없거나 한 상태를 나타낼 수 있는 타입.
  // Some: 값이 담겨져 있는 Option 의 하위 타입은 Some[T], 값이 없으면 None.
  // 승자 결정 또는 무승부 판정을 위한 논리가 담겨 있다.
  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p1.score < p2.score) Some(p2)
    else None
  // 승자를 콘솔에 출력하는 책임을 진다.
//  def contest(p1: Player, p2: Player): Unit = winner(p1, p2) match {
//    case Some(Player(name, _)) => println(s"$name is the winner!")
//    case None => println("It's a draw.")
//  }

  // 적절한 메세지를 결졍하는 책임을 가진 함수.
  def winnerMsg(p: Option[Player]): String = p map {
    case Player(name, _) => s"$name is the winner!"
  } getOrElse "It's a draw."

  // 메세지를 콘솔에 출력하는 임무를 가진 절차.
//  def contest(p1: Player, p2: Player): Unit =
//    println(winnerMsg(winner(p1, p2)))

//  trait IO {  def run: Unit }

//  def PrintLine(msg: String): IO =
//    new IO {  def run = println(msg) }

//  def contest(p1: Player, p2: Player): IO =
//    PrintLine(winnerMsg(winner(p1, p2)))

  // self 인수는 이 객체를 this 대신 self로 지징할 수 있게 한다.
//  trait IO { self =>
//    def run: Unit
//    def ++(io: IO): IO = new IO {
//      // self는 외곽의 IO를 지칭한다.
//      def run = { self.run; io.run }
//    }
//  }
//
//  object IO {
//    def empty: IO = new IO { def run = () }
//  }

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0/9.0
//
//  def converter: Unit = {
//    println("Enter a temperature in degrees Fahrenheit: ")
//    val d = readLine.toDouble
//    println(fahrenheitToCelsius(d))
//  }


//  def converter: IO = {
//    val prompt: IO = PrintLine(
//      "Enter a temperature in degrees Fahrenheit: ")
//    // 이제 어떻게 해야 할까?
//  }

  sealed trait IO[A] { self =>
    def run: A
    def map[B](f: A => B): IO[B] =
      new IO[B] { def run = f(self.run) }

    def flatMap[B](f: A => IO[B]): IO[B] = {
      println("flatMap")
      new IO[B] { def run = f(self.run).run }
    }

  }

  def ReadLine: IO[String] = IO { readLine }
  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
    def flatMap[A,B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
    // 이 매서드에 의해, 함수 적용 구문을 이용해서 IO 블록(IO { ... } 형태의)을 만들 수 있다.
    def apply[A](a: => A): IO[A] = unit(a)
    // 인수의 효과를 무한히 되풀이 한다.
    def forever[A, B](a: IO[A]): IO[B] = {
      println("forever")
      lazy val t: IO[B] = forever(a)
      a flatMap (_ => t)

    }
    // val p = IO.forever(PrintLine("Still going..."))
  }

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()
//
//  sealed trait IO[A] {
//    def flatMap[B](f: A => IO[B]): IO[B] =
//      FlatMap(this, f)
//    def map[B](f: A => B): IO[B] =
//      flatMap(f andThen (Return(_)))
//  }
//  // 댜른 작업을 진행하지 않고 즉시 A를 돌려주는 순수 계산. run에서 이 생성자를 만났다면 해당 계산이 완료된 것이다.
//  case class Return[A](a: A) extends IO[A]
//  // 계산의 일시 정지, resume은 인수를 전혀 받지 않지만 어떤 효과를 가지며 결과를 산출하는 함수이다.
//  case class Suspend[A](resume: () => A) extends IO[A]
//  // 두 단계의 합성, flatMap을 함수가 아니라 자료 생성자로서 구체화한다. run에서 이 생성자를 만났다면
//  // 먼저 부분 계산 sub를 처리하고, sub가 결과를 산출하면 k로 넘어가야 한다.
//  case class FlatMap[A,B](sub: IO[A], k: A => IO[B]) extends IO[B]

  def main(array: Array[String]): Unit = {
    // IO를 반환.
//    val printLine = PrintLine("Test PrintLine...")
//    println(printLine.getClass)
//    printLine.run
//    // IO를 반환.
//    val readLine = ReadLine
//    readLine.run

    val p = IO.forever(PrintLine("Still going..."))
    p.run
//    val p = IO2a.IO.forever(PrintLine("Still going..."))
//    println(p.getClass)


  }
}


object IO2a {

  /*
  The previous IO representation overflows the stack for some programs.
  The problem is that `run` call itself recursively, which means that
  an infinite or long running IO computation will have a chain of regular
  calls to `run`, eventually overflowing the stack.
  The general solution is to make the `IO` type into a data type that we
  interpret using a tail recursive loop, using pattern matching.
  */

  sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] =
      FlatMap(this, f) // we do not interpret the `flatMap` here, just return it as a value
    def map[B](f: A => B): IO[B] =
      flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends IO[A]
  case class Suspend[A](resume: () => A) extends IO[A]
  case class FlatMap[A,B](sub: IO[A], k: A => IO[B]) extends IO[B]

  object IO extends Monad[IO] { // Notice that none of these operations DO anything
    def unit[A](a: => A): IO[A] = Return(a)
    def flatMap[A,B](a: IO[A])(f: A => IO[B]): IO[B] = a flatMap f
    def suspend[A](a: => IO[A]) =
      Suspend(() => ()).flatMap { _ => a }
    def forever[A,B](a: IO[A]): IO[B] = {
//      lazy val t: IO[B] = a flatMap (_ => t)
      lazy val t: IO[B] = forever(a)
      a flatMap (_ => t)
    }

  }


  def printLine(s: String): IO[Unit] =
    Suspend(() => Return(println(s)))

  val p = IO.forever(printLine("Still going..."))

//  val actions: Stream[IO[Unit]] =
//    Stream.fill(100000)(printLine("Still going..."))
//  val composite: IO[Unit] =
//    actions.foldLeft(IO.unit(())) { (acc, a) => acc flatMap { _ => a } }

  // There is only one sensible way to implement this as a
  // tail-recursive function, the one tricky case is left-nested
  // flatMaps, as in `((a flatMap f) flatMap g)`, which we
  // reassociate to the right as `a flatMap (ar => f(a) flatMap g)`
  @annotation.tailrec def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }
}

object IO2aTests {
  import IO2a._

  /*
  Pg 240: REPL session has a typo, should be:
  val g = List.fill(100000)(f).foldLeft(f) {
    (a, b) => x => Suspend(() => ()).flatMap { _ => a(x).flatMap(b)}
  }
  Note: we could write a little helper function to make this nicer:
  def suspend[A](a: => IO[A]) = Suspend(() => ()).flatMap { _ => a }
  val g = List.fill(100000)(f).foldLeft(f) {
    (a, b) => x => suspend { a(x).flatMap(b) }
  }
   */

  val f: Int => IO[Int] = (i: Int) => Return(i)

  val g: Int => IO[Int] =
    List.fill(10000)(f).foldLeft(f){
      (a: Function1[Int, IO[Int]],
       b: Function1[Int, IO[Int]]) => {
        (x: Int) => IO.suspend(a(x).flatMap(b))
      }
    }

  def main(args: Array[String]): Unit = {
    val p = IO.forever(printLine("Still going..."))
    run(p)
  }
}
