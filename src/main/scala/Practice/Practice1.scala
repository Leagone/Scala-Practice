package Practice

import scala.annotation.tailrec

object Practice1 extends App {

  def abs(n: Int): Int = if (n < 0) -n else n

  def factorial(start: Int): Int = {
    def go(current: Int, acc: Int): Int =
      if (current <= 0)  acc
      else go(current - 1, current * acc)

    go(start, 1)
  }

  def fact(x: Int) : Int = {
    if (x == 0) 1
    else x * fact(x - 1)
  }

  def fib(target: Int): Int = {
    def go(prev: Int, curr: Int, count: Int): Int =
      if (count == target) curr
      else go(curr, prev + curr, count + 1)

    go(0,1,2)
  }

  def cube(x: Int) : Int = x * x * x
  def itself(x: Int) : Int = x



  def sum1(start: Int, end: Int) : Int = {
    if (start > end) 0
    else cube(start) + sum1(start + 1, end)
  }

  def sum2(f: Int => Int, current: Int, end: Int) : Int = {
    if (current > end ) 0
    else f(current) + sum2(f, current + 1, end)
  }

  @tailrec
  def sumTail(acc: Int, current: Int) : Int = {
    if (current < 0 ) acc
    else sumTail(acc + current,current - 1)
  }


  /*
  * Sum starts at start, which each next call start is incremented
  * if start is bigger than end return 0 - last recursion
  * when reach 0 go back and add to return of f(number)
  * */

  def f1(f: Int => Int) : (Int, Int) => Int = {
    def sum(start: Int, end : Int) : Int = {
      if (start > end) 0
      else f(start) + sum(start+1, end)
    }
    sum
  }

  def f2(f: Int => Int)(a: Int, b: Int) : Int = {
    if (a > b) 0 else f(a) + f2(f)(a+1 , b)
  }

  def product(f: Int => Int)(a: Int, b: Int) : Int = {
    if (a > b) 1 else f(a) + product(f)(a + 1, b)
  }

  /*
  * function takes function as parm
  *
  * */

  // f1 and f2 works the same

  val exampleRational = Rational(5,10)

  val testVal = cube(0)

}
