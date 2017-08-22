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
      if (c < 0 || r < 0 || c > r) throw new IllegalArgumentException("Incorrect : r must be greater than r, and both greater than 0") else{
        if (c == 0 || c == r) 1 else {
          pascal(c - 1, r - 1) + pascal(c, r - 1)
        }
      }
    }
  
  /**
   * Exercise 2
   */

    def balance(chars: List[Char]): Boolean = {

      def iterate(chars: List[Char], bal: Int): Boolean = {
        if (chars.isEmpty) bal == 0
        else if (bal < 0) false
        else chars.head match {
          case '(' => iterate(chars.tail, bal + 1)
          case ')' => iterate(chars.tail, bal - 1)
          case _ => iterate(chars.tail, bal)
        }
      }

      iterate(chars, 0)
    }

  /**
   * Exercise 3
   */

    def countChange(money: Int, coins: List[Int]): Int = {

      if (coins.isEmpty || money < 0) return 0
      if (money == 0) return 1
      countChange(money - coins.head, coins) + countChange(money, coins.tail)

    }
  }
