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
    def pascal(c: Int, r: Int): Int =
      if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def aux(cs: List[Char], opens: Int): Boolean = {
        if (cs.isEmpty)
          opens == 0 // 截止条件 1
        else {
          val n = if (cs.head == '(') opens + 1
                  else if (cs.head == ')') opens - 1
                  else opens
          if (n < 0) false // 截止条件 2
          else aux(cs.tail, n)
        }
      }
      aux(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def aux(money: Int, remains: List[Int]): Int = {
        if (money < 0)
          0
        else if (money == 0)
          1
        else if (remains.nonEmpty) {
            aux(money - remains.head, remains) + aux(money, remains.tail)
        } else {
          0
        }
      }
      aux(money, coins)
    }
  }
