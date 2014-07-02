import scala.collection.mutable._
import scala.util.control.Breaks._

object Test 
{
  var n = 20;
  var m = 10;
  var totalPaths = 0; 
  var isPrime: Array[Int] = new Array(10005);
  var factorial: Array[BigInt] = new Array(10005);  
  var index = 0;

  // Factorial.
  def findFactorials()
  {
    factorial(0) = 1; factorial(1) = 1;
    for(i <- 2 to 1000) factorial(i) = i * factorial(i - 1);
  }
  
  // Factorial using matching.
  def findFactorials1(n1: Int):Int = n1 match 
  {
    case 0 => 1;
    case n1 => n1 * findFactorials1(n1 - 1);
  }
  
  // Find nCr value.
  def nCr(n: Int, r: Int):BigInt = 
  {
    factorial(n) / (factorial(n - r) * factorial(r));
  }  
  
  // Sieve Technique.
  def sieve()
  {
    // Initialize
    var i = 0;
    for(i <- 0 to 10000) isPrime(i) = 1;

    isPrime(1) = 0; isPrime(0) = 0;
    for(i <- 2 to 10000 if i * i <= 100)
     if(isPrime(i) == 1)
      for(k <- i * i until 10000 by i if k <= 10000)
       isPrime(k) = 0;
  }
  
  // Print the elements of a list.
  def printValues(l: List[Int]) 
  {
    l match
    {
      case head::tail => 
        print(head + " ");
        printValues(tail);
      case Nil =>
    }
  }
  
  // [1]: Last element of the list.
  def lastElement(l: List[Int]): Int = l match
  {
    case value::Nil => value;
    case head::tail => lastElement(tail);
  }
      
  // [2]: Last but one element of the list.   
  def lastButOne(l: List[Int]): Int = l match
  {
    case value1::value2::Nil => value1;
    case head::tail => lastButOne(tail);
  } 
  
  // [3]: Reverse the list.
  def reverse(l: List[Int]): List[Int] = l match 
  {
    // Concatenate the single element list with the remaining portion backwards.    
    case Nil => l;
    case head::tail => reverse(tail):::List(head);
  }
  
  // [4]: Eliminate consecutive duplicates of list elements.
  def eliminate(l: List[Int]): List[Int] = l match
  {
    case head::tail => head::eliminate(tail.dropWhile(_ == head));
    case Nil => l;
  }
  
  // [5]: Duplicate the elements of a list a given number of times.
  def duplicateN(l: List[Int], n: Int): List[Int] = 
  {
    l.flatMap(value => (1 to n).map(_ => value));
  }
  
  // Square the elements of a list.
  def squareList(l: List[Int]): List[Int] = 
  {
    l.map(value => value * value);
  }  
  
  // [6]: Drop every n'th element from the list.
  def drop(l: List[Int], n: Int): List[Int] = dropRecursive(l, n, 1);
  
  // Drop Recursive.
  def dropRecursive(l: List[Int], n: Int, current: Int): List[Int] = 
  (l, n, current) match 
  {
    case (head::tail, n, current) if current % n != 0 => head::dropRecursive(tail, n, current + 1);
    case (head::tail, n, current) if current % n == 0 => dropRecursive(tail, n, current + 1);    
    case (Nil, _, _) => Nil;
  }
  
  // [7]: Extract a slice from the list.
  def slice(l: List[Int], a: Int, b: Int): List[Int] = sliceRecursive(l, a, b, 0);
  
  // Slice Recursive.
  def sliceRecursive(l: List[Int], a: Int, b: Int, index: Int): List[Int] = 
  (l, a, b, index) match 
  {
    case (head::tail, a, b, index) if index >= a && index <= b => head::sliceRecursive(tail, a, b, index + 1);
    case (head::tail, a, b, index) if index < a || index > b => sliceRecursive(tail, a, b, index + 1);    
    case (Nil, _, _, _) => Nil;
  }  
  
  // [8]: A list of prime numbers.
  def listPrimesinRange(left: Int, right: Int)
  {
    def printPrimes(value: Int)
    {
      if(isPrime(value) == 1 && value >= left && value <= right)
      {
        print(value + " ");
        printPrimes(value + 1);
      } 
      else if(value <= right)
      {
        printPrimes(value + 1);        
      }
    }
    
    sieve();
    printPrimes(1);
  }
  
  // [9]: Sum of all multiples of 3 or 5 below 1000.
  def multiplesSum(n: Int): Int = 
  {
    n match
    {
      case value if value >= 1 && (value % 3 == 0 || value % 5 == 0) => value + multiplesSum(n - 1);
      case value if value >= 1 && !(value % 3 == 0 || value % 5 == 0) => multiplesSum(n - 1);      
      case 0 => 0;
    }
  }  
  
  // [10]: Match digit to word.
  def getMatch(d: Int) = d match
  {
    case 1 => "one";
    case 2 => "two";
    case 3 => "three";
    case 4 => "four";
    case 5 => "five";
    case 6 => "six";
    case 7 => "seven";
    case 8 => "eight";
    case 9 => "nine";
    case 0 => "zero";
  }
  
  // [10]: English number words.
  def fullWords(number: Int)
  {
    var n = number;
    var lb = new ListBuffer[String];
    
    while(n > 0)
    {
      val digit: Int = n % 10;
      lb += getMatch(digit);
      
      n = n / 10;
    }
    
    lb = lb.reverse;
    print(lb(0));
    for(i <- 1 to lb.length - 1) print("-" + lb(i));
    println;
  }
  
  // [11]: Lattice Paths.
  def latticePaths1(i: Int, j: Int)
  {
    if(i > n || j > m) return;
    if(i == n && j == m) totalPaths += 1;
    latticePaths1(i + 1, j);
    latticePaths1(i, j + 1);
  }
  
  // [11]: Lattice Paths.
  def latticePaths2(n1: Int, m1: Int): BigInt =
  {
    if(n1 >= m1) nCr(n1 + m1 - 2, m1 - 1);
    else nCr(n1 + m1 - 2, n1 - 1);
  }
  
  // Currying example for string concatenation.
  def concat(s1: String)(s2: String): String = s1 + s2;
  def concatMultiple(s1: String)(s2: String)(s3: String) = concat(s1)(s2) + s3;
  
  def main(args: Array[String])
  {    
    var l: List[Int] = List(1, 2, 3, 4, 5, 6, 7);
    //var l: List[Int] = List(1, 2, 5);
    //findFactorials()
    
    //println(lastElement(l));
    //println(lastButOne(l));
    
    //println(reverse(l));
    
    //println(eliminate(l));
    //println(duplicateN(l, 2));
    
    //println(l.foldLeft(0)(_ + _)); // Sum of the elements in the list.
    //println(l.foldLeft(0)((sum, _) => sum + 1)); // Number of elements in the list.
    //println(l.foldLeft(0)(_ + _) / l.foldLeft(0)((sum, _) => sum + 1)); // Average of all the elements in the list.
    
    //printValues(l);
    //println(squareList(l));
    
    //println(drop(l, 3));
    //println(slice(l, 0, 3));
    
    //listPrimesinRange(1, 100);
    //println(multiplesSum(10));
    //fullWords(6789123);
    
    //latticePaths1(1, 1);    
    //println("Total Paths = " + totalPaths);    
    //println("Total Paths = " + latticePaths2(20 + 1, 20 + 1));
    
    //println("7! is " + findFactorials1(7));
    
    println(concatMultiple("one")("two")("three"));
  }
}
