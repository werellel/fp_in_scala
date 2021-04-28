package ch07

import java.util.concurrent._

object Par {
  /* unit은 UnitFuture를 돌려주는 함수로 표현된다. UnitFuture는 Future의 간단한 구현으로,
  그냥 상수 값을 감싸기만 할 뿐 ExcutorService는 전혀 사용하지 않는다. UnitFuture는 항상 완료 가능하며, 취소는 불가능하다.
  UnitFuture의 get 메서드는 이전에 주어진 상수 값을 돌려주기만 한다.
  */
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  /* 이 API에서 병렬성 제어는 오직 fork 함수만 담당한다는 설계상의 선택에 따라, map2는 f 호출을 개별 논리적 스레드에서 평가하지 않는다.
  f를 개별 스레드에서 평가하고 싶다면 fork(map2(a,b)(f))를 사용하면 된다.
  */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      /* map2의 이 구현은 만료 시간을 지키지 않는다. 이 구현은 그냥 ExcutorService를 두 Par 값에 전달하고, Future의 af와 bf의 결과들을 기다리고,
      그것들에 f를 적용하고, 적용 결과를 UnitFuture로 감쌀 뿐이다. 만료 시간을 지키기 위해서는, af의 평가에 걸린 시간을 측정하고 bf의 평가에 걸린 시간에서 그 시간을
      뺴는 식의 새로운 Future 구현이 필요할 것이다.
      */
      UnitFuture(f(af.get, bf.get))
    }

  /* 이것이 fork의 가장 간단하고 가장 자연스러운 구현이나, 몇 가지 문제점이 있다. 예를 들어 외각의 Callable은 '안쪽' 과제가 완료될 때까지 차단된다.
  이러한 차단이 스레드 풀의 한 스레드를 점유하며, 이는 잠재적 병렬성의 일부가 소실될 수 있음을 의미한다. 본질적으로 이 구현은 한 스레드로 충분한 작업을 두 새의 스레드로 수행한다.
  이 구현의 좀 더 심각한 문제점의 증상이다.
  */
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))
}
