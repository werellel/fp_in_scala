package ch04

class FailingFn {
  // val y: Int = ...는 변수 y의 형식이 Int이고 그 값은 '='의 우변과 같음을 선언
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    }
    // catch 블록은 패턴 부합과 다를 바 없다.
    // case e: Exception은 임의의 Exception과 부합하는 패턴
    // 부합하는 값은 43을 돌려준다.
    catch { case e: Exception => 43 }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      // 던질 Exception에 그 어떤 형식이라도 지정할 수 있다.
      // 여기에서는 형식 주해로 Int를 지정.
      x + ((throw new Exception("fail!")): Int)
    }
    catch { case e: Exception => 43 }
  }
}
