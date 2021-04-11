//package ch02
// 이것은 주석!
/* 이것도 주석 */
/** 문서화도 주석 **/
// '{' 단일체 객채의 선언, 클래스와 클래스의 유일한 인스턴스를 동시에 선언
object MyModule {
  // abs는 정수 하나를 받고 정수를 돌려준다.
  def abs(n:Int): Int =
    if (n < 0) -n
    else n

  // private 메소드는 오직 MyModule의 다른 멤버들만 호출할 수 있다.
  private def formatAbs(x: Int) = {
    // 수치를 위한 자리표 : %d
    val msg = "The absolute value of %d is is %d"
    msg.format(x, abs(x))
  }
  // Unit은 Java나 C같은 언어의 void와 같은 목적으로 쓰인다.
  /** main이라는 이름은 특별하다. 프로그램을 실행할 때 스칼라가 특정한 서명을 가진 main이라는
  이름의 메서드를 찾기 때문. 좀 더 구체적으로, main 메서드는 반드시 String들의 Array를 인수로 받아야 하며
  반환 형식은 반드시 Unit이어야 한다.
  args 배열에는 프로그램 실행 시 명령줄에 지정한 인수들이 들어 있다.
  반환 형식이 Unit이라는 것은 그 메서드에 부수 효과가 존재함을 암시한다.
   *
   */
  def main(args: Array[String]): Unit =
    println(formatAbs(-42))

  def factorial(n: Int): Int = {
    // 내부 함수 또는 지역 정의 스칼라에서는 한 함수의 본문에 지역적인 또 다른
    // 함수를 작성하는 일이 흔하다. 함수형 프로그래밍에서 이런 함수는 지역 정수나 문자열과 다를 바 없는 값이다.
    // 스칼라에서 루프용 보조 함수에는 go나 loop 같은 이름을 붙이는 것이 관례이다.
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
    go(n, 1)
  }
}

/** object 키워드는 새로운 단일체(Singleton) 형식을 만든다.
단일체는 class와 비슷하되 명명된 인스턴스가 단 하나라는 점이 다르다. Java의 익명 클래스의
새 인스턴스를 생성하는 것과 아주 비슷하다고 생각하면 될 것이다.

스칼라에는 Java의 static 키워드에 해당하는 것이 없으며, Java에서 정적 멤버를 가진 클래스를 사용할 만한
상활일 때 스칼라에서 흔히 object를 사용.
**/
