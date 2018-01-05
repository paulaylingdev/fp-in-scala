object MyModule {
  def abs(n: Int): Int = 
    if (n < 0) -n
    else n
    
  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = 
      if (n <= 0) acc
      else go(n-1, n*acc)
    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int = {
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    }
    loop(n, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(factorial(3))
    println(fib(10))
  }
}
