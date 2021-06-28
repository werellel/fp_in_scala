package ch14

sealed trait ST[S,A] { self =>
  protected def run(s: S): (A,S)
  def map[B](f: A => B): ST[S,B] = new ST[S,B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }
  def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S,A](a: => A) = {
    lazy val memo = a
    new ST[S,A] {
      def run(s: S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A =
    st.apply[Unit].run(())._1
}

sealed trait STRef[S,A] {
  protected var cell: A
  def read: ST[S,A] = ST(cell)
  def write(a: A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S,A](a: A): ST[S, STRef[S,A]] = ST(new STRef[S,A] {
    var cell = a
  })
}
trait RunnableST[A] {
  def apply[S]: ST[S,A]
}


object main {
  def quicksort(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
    val arr = xs.toArray
    def swap(x: Int, y: Int) = {
      val tmp = arr(x)
      arr(x) = arr(y)
      arr(y) = tmp
    }
    def partition(n: Int, r: Int, pivot: Int) = {
      val pivotVal = arr(pivot)
      swap(pivot, r)
      var j = n
      for (i <- n until r) if (arr(i) < pivotVal) {
        swap(i, j)
        j += 1
      }
      swap(j, r)
      j
    }
    def qs(n: Int, r: Int): Unit = if (n < r) {
      val pi = partition(n, r, n + (n - r) / 2)
      qs(n, pi - 1)
      qs(pi + 1, r)
    }
    qs(0, arr.length - 1)
    arr.toList
  }

  def main(array: Array[String]): Unit = {
//    val ref1 = STRef[Nothing, Int](1)
//    println(ref1)
//    val p = new RunnableST[(Int, Int)] {
//      def apply[S] = for {
//        r1 <- STRef(1)
//        r2 <- STRef(2)
//        x <- r1.read
//        y <- r2.read
//        _ <- r1.write(y+1)
//        _ <- r2.write(x+1)
//        a <- r1.read
//        b <- r2.read
//      } yield (a,b)
//    }
//    val r = ST.runST(p)
//    println(r)
//    val temp_list = List(1,5, 4, 2, 1)
//    val sortedList = quicksort(temp_list)
//    println(sortedList)
    new RunnableST[STRef[Nothing,Int]] {
      def apply[S] = STRef(1)
    }

  }
}