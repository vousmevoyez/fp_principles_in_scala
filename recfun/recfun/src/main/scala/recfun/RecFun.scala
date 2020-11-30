package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @scala.annotation.tailrec
    def balanceIter(acc: Int, chars: List[Char]): Int = {
      if (chars.isEmpty || acc < 0)  acc
      else if (chars.head == '(') balanceIter(acc + 1, chars.tail)
      else if (chars.head == ')') balanceIter(acc - 1, chars.tail)
      else balanceIter(acc, chars.tail)
    }
    balanceIter(0, chars) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
//    var dp = Array.fill(money+1)(0)
    var dp = Array.ofDim[Int](money+1)
    dp(0) = 1
    for (i <- coins.indices) {
      for (j <- coins(i) until money+1) {
        dp(j) += dp(j-coins(i))
      }
    }
    dp(money)
  }
}
