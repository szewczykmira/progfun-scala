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
    That is (row column)
   */
    def pascal(column: Int, row: Int): Int = {
      def factor(acc: Int, elem:Int) : Int = 
        if (elem == 1) acc else factor(acc*elem, elem-1)
      
      if (column == 0 || column == row) 1 else
        factor(1, row) / (factor(1, column) * factor(1, row-column))
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def charToInt(char: Char): Int =  char match {
        case '(' => 1
        case ')' => -1
        case _ => 0
      }
      
      def parse(acc: Int, chars: List[Char]) : Int = {
        if(chars.isEmpty) acc else
          if(acc < 0) -1 else parse(acc + charToInt(chars.head), chars.tail)
      }
      
      if(parse(0, chars) == 0) true else false 
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def count(elem:Int, coins:List[Int], acc:Int, lista:List[Int]): Int = {
        if(coins.isEmpty) 
          acc
        else
          if (elem < coins.head)
            count(elem, coins.tail, acc, lista)
          else
            count(elem, coins.tail, acc + lista(coins.head - 1), lista)
      }
      
      def fill(elem: Int, lista:List[Int]) : List[Int] = {
        if(elem == money)
          count(elem, coins, 0, lista) :: lista
        else
          fill(elem+1, count(elem, coins, 0, lista) :: lista)
      } 
      if (money == 0) 
        1 
      else 
        fill(1, List(1))(0)
    }
  }
