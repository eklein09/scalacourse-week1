package recfun

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
      if ((r==0) || (r==c) || (c==0))
        1
      else
        pascal(c,r-1) + pascal(c-1,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def countParensAccum(wasBelowZero: Boolean, accum: Int, chars: List[Char]): (Boolean, Int) = {

        if (chars.isEmpty)
          (wasBelowZero,accum)
        else if (chars.head == '(')
          countParensAccum(wasBelowZero,accum+1,chars.tail)
        else if (chars.head == ')')
          countParensAccum((accum-1 < 0) || wasBelowZero, accum-1,chars.tail)
        else
          countParensAccum(wasBelowZero, accum, chars.tail)

      }
      val result = countParensAccum(false, 0, chars)
      !result._1 && (result._2==0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if ((money <= 0) || coins.isEmpty)
        0
      else {
          if (money % coins.head == 0)
            1 + countChange(money, coins.tail) + countChange(money - coins.head, coins.tail)
          else
            countChange(money - coins.head, coins.tail) + countChange(money, coins.tail)
        }
    }
  }
