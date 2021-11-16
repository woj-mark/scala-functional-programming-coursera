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
   * Use tail recursion with a helper function
   */
  def balance(chars: List[Char]): Boolean = ???

  /**
   * Exercise 
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
