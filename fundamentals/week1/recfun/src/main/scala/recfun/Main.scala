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
      if(c==0 || r==0 || c==r)
        1
      else
        pascal(c-1, r-1) + pascal(c, r-1)


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    if(chars.isEmpty) throw new IllegalArgumentException("Empty string")

    def innerBalance(chars: List[Char], opens: Int): Boolean =
      if(opens < 0)
        false
      else
        if(chars.isEmpty)
          opens == 0
        else
          if(chars.head == '(')
            innerBalance(chars.tail, opens+1)
          else
            if(chars.head == ')')
              innerBalance(chars.tail, opens-1)
            else
              innerBalance(chars.tail, opens)

    innerBalance(chars, 0)
  }


  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money == 0)
        1
      else
        if(coins.isEmpty && money > 0)
          0
        else
          if(money < 0)
            0
          else
            countChange(money-coins.head, coins) + countChange(money, coins.tail)
    }
  }
