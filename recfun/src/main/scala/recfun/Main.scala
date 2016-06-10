package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (r < 0 || c < 0) 0 else
    if (r == 0 && c == 0 ) 1 else
    pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def verify(chars: List[Char], count: Int): Boolean = {
      if (count < 0 || (chars.isEmpty && count > 0)) false else
      if (chars.isEmpty && count == 0) true else {
        val ch = chars.head
        if ( ch == '(' ) verify(chars.tail, count + 1) else
        if ( ch == ')' ) verify(chars.tail, count - 1) else
        verify(chars.tail, count)
      }
    }
    verify(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if ( money == 0 ) 1 else
    if ( money < 0 ) 0 else
    if ( coins.isEmpty ) 0 else {
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
}
