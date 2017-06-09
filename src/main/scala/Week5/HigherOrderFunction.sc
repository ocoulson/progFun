def scaleList(xs: List[Int], factor: Int): List[Int] = {
  xs map (x => x * factor)
}

scaleList(List(23,1,34,553),2)

def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y::ys => (y*y)::squareList(ys)
}

def squareList2(xs: List[Int]): List[Int] = xs map (x => x*x)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x::xs1 =>
    var (first, rest) = xs.span(_==x)
    first :: pack(rest)
}

val data = List("a", "a", "a", "b", "c", "c", "a")
pack(data)


def encode[T](xs: List[T]): List[(T, Int)] = {
  pack(xs) map ((list: List[T] ) => (list.head, list.length))
}

encode(data)


def mapFun[T,U](xs: List[T], f: T => U): List[U] = {
  (xs foldRight List[U]()) ((x: T, ys: List[U]) => f(x)::ys)
}
def lengthFun[T,U](xs: List[T]): Int = {
  (xs foldRight 0)((x: T, y: Int) => y + 1)
}
mapFun(List(1,2,3,4), (x: Int )=> x * x)

lengthFun(List(1,2,3,4,5))