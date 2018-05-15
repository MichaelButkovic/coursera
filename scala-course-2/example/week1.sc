val f: String => String = { case "ping" => "pong" }
f("ping")

val f1: PartialFunction[String, String] = { case "ping" => "pong" }
f1.isDefinedAt("abc")
f1.isDefinedAt("ping")

val f2: PartialFunction[List[Int], String] = {
  case Nil => "one"
  case x :: y :: rest => "two"
}
f2.isDefinedAt(List(1, 2, 3))

val g: PartialFunction[List[Int], String] = {
  case Nil => "one"
  case x :: rest =>
    rest match {
      case Nil => "two"
    }
}
g.isDefinedAt(List(1, 2, 3)) // .isDefinedAt() only checks outermost matching

// for (x <- e1) yield e2 is translated to
// e1.map(x => e2)

// for (x <- e1 if f; s) yield e2 is translated to
// for (x <- e1.withFilter(x => f); s) yield e2

// for (x <- e1; y <- e2; s) yield e3 is translated to
// e1.flatMap(x => for (y <- e2; s) yield e3)

case class Book(title: String, authors: List[String])

val books: List[Book] = List(
    Book(title = "Whatever",
         authors = List("Who, Cares"))
)

// title of books whose author's name is "Bird"
for (b <- books; a <- b.authors if a startsWith "Bird,") yield b.title

// all books which have the word "Program" in the title
// for (b <- books if b.title indexOf "Program" >= 0) yield b.title

// all authors who have written at least two books
for {
  b1 <- books
  b2 <- books
  if b1 != b2
  a1 <- b1.authors
  a2 <- b2.authors
  if a1 == a2
} yield a1

for {
  b1 <- books
  b2 <- books
  if b1.title < b2.title
  a1 <- b1.authors
  a2 <- b2.authors
  if a1 == a2
} yield a1

{
  for {
    b1 <- books
    b2 <- books
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1
}.distinct

// OR

val books2 = Set(
  Book(title = "Whatever",
    authors = List("Who, Cares"))
)


// for (x <- e1) yield e2 is translated to
// e1.map(x => e2)

// for (x <- e1 if f; s) yield e2 is translated to
// for (x <- e1.withFilter(x => f); s) yield e2

// for (x <- e1; y <- e2; s) yield e3 is translated to
// e1.flatMap(x => for (y <- e2; s) yield e3)

//for {
//  i <- 1 until n
//  j <- 1 until i
//  if isPrime(i + j)
//} yield (i, j)
//  is translated to
//
//(1 until n).flatMap(i =>
//  (1 until i).withFilter(j => isPrime(i+j))
//    .map(j => (i, j)))

trait Generator[+T] {
  self =>

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    def generate = f(self.generate).generate
  }
}

val integers = new Generator[Int] {
  val rand = new java.util.Random
  def generate = rand.nextInt()
}

val booleans = new Generator[Boolean] {
  def generate = integers.generate > 0
}

val pairs = new Generator[(Int, Int)] {
  def generate = (integers.generate, integers.generate)
}

// val booleans2 = for (x <- integers) yield x > 0
// val booleans3 = integers map { x => x > 0 }
// val booleans4 = new Generator[Boolean] {
//   def generate = (x: Int => x > 0)(integers.generate)
// }
// val booleans5 = new Generator[Boolean] {
//   def generate = integers.generate > 0
// }


def single[T](x: T): Generator[T] = new Generator[T] {
  def generate = x
}

def choose(lo: Int, hi: Int): Generator[Int] =
  for (x <- integers) yield lo + x % (hi - lo)

def oneOf[T](xs: T*): Generator[T] =
  for (idx <- choose(0, xs.length)) yield xs(idx)

def lists: Generator[List[Int]] = for {
  isEmpty <- booleans
  list <- if (isEmpty) emptyLists else nonEmptyLists
} yield list

def emptyLists = single(Nil)

def nonEmptyLists = for {
  head <- integers
  tail <- lists
} yield head :: tail

//def leafs: Generator[Leaf] = for {
//  x <- integers
//} yield Leaf(x)
//
//def inners: Generator[Inner] = for {
//  l <- trees
//  r <- trees
//} yield Inner(l, r)
//
//def trees: Generator[Tree] = for {
//  isLeaf <- booleans
//  tree <- if (isLeaf) leafs else inners
//} yield tree