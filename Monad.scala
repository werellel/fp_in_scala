package ch11

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }

}
//trait Mon[F[_]] {
//  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
//    fa flatMap (a => fb map (b => f(a,b)))
//}

trait Mon[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

  def map2[A,B,C](
                   fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a,b)))
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

}


object main{
  def main(array: Array[String]): Unit = {
    val testList = List(1, 2, 3)

    def testF(i: Int): Int ={
      i * 2
    }

    val testResult = testList map testF
    println("testResult: ", testResult)

    val listFunctor = new Functor[List] {
      def map[A, B](as: List[A])(f: A => B): List[B] = as map f
    }

    val testFunctorNums: List[Int] = List(1, 2, 3, 4)
    val resulMaptTestFunctorNums = listFunctor.map(testFunctorNums)(x => x*2)
    println(resulMaptTestFunctorNums)

    val distributetTestFunctorNums = List(("Mercury", 57.9), ("Venus", 108.2), ("Earth", 149.6),
                                          ("Mars", 227.9), ("Jupiter", 778.3)
    )

    val resultDistributetTestFunctorNums = listFunctor.distribute(distributetTestFunctorNums)
    println(resultDistributetTestFunctorNums)


    def eitherTest(num: Option[Int]): Either[List[Int], List[Int]] = num match {
      case Some(n) => Right(List(1, 2, 3, 4, 5))
      case None => Left(List(6, 7, 8, 9, 0))
    }

    val codistributetTestFunctorNums = eitherTest(None)
    val resultCodistributetTestFunctorNums = listFunctor.codistribute(codistributetTestFunctorNums)
    println(resultCodistributetTestFunctorNums)

//    val optionMonad = new Monad[Option] {
//      def unit[A](a: => A) = Some(a)
//
//      override def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
//    }

    val listMonad = new Monad[List] {
      def unit[A](a: => A) = List(a)

      override def flatMap[A, B](ma: List[A])(f: A => List[B]) = ma flatMap f
    }
    val testReplicateMList = List(1, 2, 3)
    println("replicateM of listMonad: ", listMonad.replicateM(2, testReplicateMList))

  }
}

