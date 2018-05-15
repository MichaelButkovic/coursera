val xs = Array(1, 2, 3, 44)
xs map (x => x * 2)

val st = "Hello World"
st filter (c => c.isUpper)
st exists (c => c.isUpper)
st forall (c => c.isUpper)

val pairs = List(1, 2, 3) zip s
pairs.unzip

s flatMap (c => List('.', c))

val r: Range = 1 until 5
val r2: Range = 1 to 5
val r3 = 1 to 10 by 3
val r4 = 6 to 1 by -2

/*
  xs exists p
  xs forall p
  xs zip ys
  xs.unzip
  xs.flatMap
  xs.sum
  xs.product
  xs.max
  xs.min
 */

val n = 7
//(1 until n) flatMap (i => (1 until i) map (j => (i, j))) filter (pair => isPrime(pair._1 + pair._2))

//for {
//  i <- 1 until n
//  j <- 1 until i
//  if isPrime(i + j)
//} yield (i, j)


val fruit = Set("apple", "banana", "pear")
val s = (1 to 6).toSet

s map (_ + 2)
// fruit filter (_.startsWith == "app")
s.nonEmpty


def queens(n: Int): Set[List[Int]] = {
  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    queensWithRow forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }

  def placeQueens(k: Int): Set[List[Int]] = {
    if (k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens
  }
  placeQueens(n)
}

def show(queens: List[Int]) = {
  val lines =
    for (col <- queens.reverse)
    yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  "\n" + (lines mkString "\n")
}

(queens(4) map show) mkString "\n"


val roman = Map('I' -> 1, 'V' -> 5, 'X' -> 10)
val capital = Map("US" -> "Washington")
capital("US")

