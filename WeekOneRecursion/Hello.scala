import scala.annotation.tailrec
import scala.collection.mutable.Stack

object HelloWorld {
  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x))

    def abs(x: Double) = if(x<0) -x else x

    def isGoodEnough(guess: Double, x:Double) =
      abs(guess * guess-x) < 0.001

    def improve(num: Double, num2: Double) =
      (num+num2/num) / 2

    sqrtIter(1.0)
  }

 // tail recursion - function's stack frame is reused
 // one stack frame is sufficient for both functions
 @tailrec
 def gcd(a : Int, b: Int): Int =
   if (b == 0) a else gcd(b, a % b)

 // non-tail recursive
 def factorial(n: Int): Int =
   if (n == 0) 1 else n * factorial(n-1)

 // using tail recursion
 def factorial_tail(n: Int): Int = {
   @tailrec
   def helper(currNum: Int, product: Int): Int =
     if (currNum == 0) product
     else helper(currNum-1, product * currNum)
   helper(n, 1)
 }

 //  calculating rth value in nth row of pascal's triangle
 def pascal(col: Int, row: Int): Int = {
   if ( col == 0 || col == row || row == 0) 1
   else pascal(row-1, col-1) + pascal(row, col-1)
 }

 def balance(chars: List[Char]): Boolean = {
   def helper(chars: List[Char], stack: Stack[Char], index: Int): Boolean = {
     if (index == chars.length-1 )
       if (stack.length == 0) true else false
     else if (chars(index) == '(')
       helper(chars, stack.push('('), index+1)
     else if (chars(index) == ')'){
       if (stack(stack.length-1) == '('){
         stack.pop
         helper(chars, stack, index+1)
       }
       else
         false
    }
    else helper(chars, stack, index+1)
  }


   val stack = new scala.collection.mutable.Stack[Char]
   helper(chars, stack, 0)
 }

 def countChange(money: Int, coints: List[Int]): Int = {
   def helper(money: Int, coints: List[Int]): Int =
     if (money == 0) 1
     else if (money < 0 || coints.isEmpty) 0
     else{
       var numWays = 0
       for ( i <- 0 to money/coints.head)
         numWays += helper(money - i*coints.head, coints.tail)
       numWays
     }
   helper(money, coints)
 }


  def main(args: Array[String]): Unit = {
    // println(sqrt(2))
    // println(sqrt(4))
    // println(gcd(7,21))
    // println(factorial_tail(6))
    // println(pascal(7,100))
    println(countChange(10,List(1,5,10,25)))
  }

}
