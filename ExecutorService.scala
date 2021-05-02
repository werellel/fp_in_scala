package ch07

import java.util.concurrent.TimeUnit
//import java.util.concurrent._



class ExecutorService {
  def submit[A](a: Callable[A]): Future[A]
}
// 사실상 그냥 게으른 A임.
trait Callable[A] { def call: A}
trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}