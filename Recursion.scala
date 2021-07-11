package Assignments

object recursion {

  def gcd(n1:Int,n2:Int):Int=n2 match {
    case 0 => n1
    case x if x > n1 => gcd(x, n1)
    case x => gcd(x, n1 % x)
  }

  def prime(n1:Int,n:Int=2):Boolean=n match {
    case x if x==n1 => true
    case x if gcd(n1,x)>1 => false
    case x => prime(n1,x+1)
  }

  def primeSeq(n1:Int,n:Int=2):Unit={
    if (n<n1 && prime(n))
      print(n+"\t")
    if (n+1<n1)
      primeSeq(n1,n+1)
  }

  var res = 0
  def sum(n:Int):Int ={
    if (n == 1) return 1
    else res = n + sum(n - 1)
    res
  }

  def isEven(n:Int):String={
    if (n == 0) return "even";
    else if (n == 1) return "odd";
    else return isEven(n - 2);
  }

  def sumEven(n:Int):Int= {
    n match{
      case x if (x<2) => 0
      case 2 => 2
      case x if (isEven(x) =="even") => x + sumEven(x-1)
      case x => sumEven(x-1)
    }
  }

  def fib(n:Int):Int= n match{
    case 0 => 0
    case 1 => 1
    case _ => fib(n-1)+fib(n-2)
  }

  def fibSeq(n:Int):Unit= {
    if (n > 0) fibSeq(n-1)
    print(fib(n)+"\t")
  }

  def main(args:Array[String]):Unit = {
    println("Prime(5) - "+ prime(5))
    println("Prime(8) - "+ prime(8))
    print("PrimeSeq(10) - ")
    primeSeq(10)
    println("\nSum(5) - " + sum(5))
    println("5 is " + isEven(5))
    println("8 is " + isEven(8))
    println("Sum of evens less than 5- " + sumEven(5))
    print("Fibonacci sequence of 8 terms - ")
    print(fibSeq(8))
  }
}
