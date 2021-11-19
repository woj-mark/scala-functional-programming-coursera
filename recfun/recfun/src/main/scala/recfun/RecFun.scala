package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def helpPascal(col: Int, row: Int, acc:Int =1) : Int = {
      if (col == 0 || col == row) acc
      else pascal(col-1,row-1) + pascal(col,row-1)
  }
    helpPascal(c,r)
}
    //Implementing without tailrec
    //if (c == 0 || c == r) 1
    //else  pascal(c-1,r-1) + pascal(c,r-1)


  /**
   * Exercise 2
   * Use tail recursion with a helper function. Traverse the string from left to right
   * 1. Add a helper function with (list, right panatheses, left parantheses)
   * 2.Cases:
     a) if empty -> 
     b) left parantheses
     c) right parantheses

     if (left == right )
   */
  def balance(chars: List[Char]): Boolean = {
    def countBrackets(chars: List[Char], openBracket:Int =0) :Boolean ={
      if (chars == Nil) openBracket ==0 //End of recursion, checking if balances out

      else chars.head match{
        case '(' => countBrackets(chars.tail, openBracket + 1)
        case  ')' => if (openBracket > 0) countBrackets(chars.tail, openBracket - 1) else false
        case _ => countBrackets(chars.tail, openBracket) //go to the next
      }}
      countBrackets(chars)
    }



  /**
   * Exercise 
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
