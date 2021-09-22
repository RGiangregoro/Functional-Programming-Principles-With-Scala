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
      //If column is 0 or = row should return 1,
      //--otherwise recursively add the ones in the row above
      if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      //Function that will return 0 when chars is empty, add 1 to open when ( and -1 when ).
      //That is what is checking the matching parenthesis
      def pBalancing(chars: List[Char], open: Int): Boolean =
        if(chars.isEmpty) open == 0
        else if(chars.head == '(') pBalancing(chars.tail, open + 1)
        else if (chars.head == ')') open > 0 && pBalancing(chars.tail, open - 1)
        else pBalancing(chars.tail, open)
      pBalancing(chars,0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      //If coins is empty or money is less than 0 return 0
      //If money is 0 there is only 1 way to get that, with no coins so return 1
      //Otherwise recursively add countChange with the coins tail
      //--and countChange with money - coins head
      if(coins.isEmpty || money < 0) 0
      else if (money == 0) 1
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
