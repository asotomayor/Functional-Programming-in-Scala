package example

object Lists {
  def main(args: Array[String]): Unit = {
    // Function call
    val xs = List.range(1, 10)
    //val xs: List[Nothing] = List()
    println("The sum value is: " + sum(xs))
    println("The max value is: " + max(xs))
  }


  def sum(xs: List[Int]): Int = {
    if(xs.isEmpty) 0
    else xs.head + sum(xs.tail)
  }

  def max(xs: List[Int]): Int = {
    if (xs.isEmpty) {
      throw new NoSuchElementException("empty list")
    }
    def iter(xs: List[Int], currMax: Int): Int = {
      if(xs.isEmpty) {
        currMax
      }
      else {
        iter(xs.tail, if(xs.head > currMax) xs.head else currMax)
      }
    }
    iter(xs.tail, xs.head)
  }
}
