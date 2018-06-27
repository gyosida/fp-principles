package lessons.week6

 object NestedSequences {

   // given a positive integer n, find all pairs of positive
   // integers i and j, with i <= j < i < n such that
   // i + j is prime

   def isPrime(n: Int): Boolean =
     (2 until n) forall (x => n % x != 0)

   val n = 7

   // using high order functions
   (1 until n) flatMap  (i =>
     (1 until i) map (j => (i, j))) filter { case (a, b) => isPrime(a + b) }

   // using for expression
   for {
     i <- 1 until n
     j <- 1 until i
     if isPrime(i + j)
   } yield (i, j)

   // define a scalar product function using for expression

   def scalarProduct(xs: List[Double], ys: List[Double]): Double =
     (for { (x, y) <- xs zip ys } yield x * y).sum

 }
