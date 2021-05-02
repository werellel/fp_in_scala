package ch07

class example {
  /* Seq는 표준 라이브러리에 있는 목록과 기타 순차열들의 상위 클래스다.
  포인트는 Seq에 foldLeft 메서드가 있다.
  */
  def sum(ints: Seq[Int]): Int =
    ints.foldLeft(0)((a, b) => a + b)

  /* IndexedSeq는 표준 라이브러리의 Vector와 비슷한 임의 접근 순차열들의 상위 클래스다.
  목록과는 달리 이런 순차열은 순차열을
  특정 지점에서 두 부분으로 분할하는 효율적인 splitAt 메서드를 제공.
   */
  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      // headOption은 스칼라의 모든 컬렉션에 정의되는 메서드다.
      ints.headOption getOrElse 0
    else {
      // splitAt 함수를 이용해서 순차열을 반으로 나눈다.
      val (l, r) = ints.splitAt(ints.length/2)
      sum(l) + sum(r)
    }

  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      // 왼쪽 절반을 병렬로 계산.
      val sumL: Par[Int] = Par.unit(sum(l))
      // 오른쪽 절반을 병렬로 계산.
      val sumR: Par[Int] = Par.unit(sum(r))
      // 두 결과를 추출해서 합한다.
      Par.get(sumL) + Par.get(sumR)
    }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      Par.map2(sum(l), sum(r))(_ + _)
    }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

}
