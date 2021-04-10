package ch01

class CreditCard {
  def charge(price: Int): Int = {
    5
  }
}
class Coffee {
  val price = 3
}
// 자바에서 처럼 class 키워드는 클래스를 도입한다. 중괄호 사이는 클래스의 body
class Cafe {
  // 클래스 메서드는 def 키워드로 도입
  def buyCoffee(cc: CreditCard): Coffee = {
    // 세미콜론 없어도 된다. 한 블록 안의 문장들은 새 줄로 구분
    val cup = new Coffee()
    // 부수 효과. 신용카드를 실제고 청구한다.
    cc.charge(cup.price)
    // return이 없어도 된다. cup이 블록의 마지막 문장이므로 자동으로 반환.
    cup
  }
}

object Ch01 extends App {
  println("Ch01")
}
