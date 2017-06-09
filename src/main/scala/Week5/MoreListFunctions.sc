def last[T](xs: List[T]) : T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case y::ys => last(ys)
}

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => List(y) ++ init(ys)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z::zs => z::concat(zs,ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => List()
  case y::ys => reverse(ys) ++ List(y)
}

def removeAt[T](xs: List[T], n: Int): List[T] = {
  if (xs.length-1 < n) xs
  else (xs take n) ::: (xs drop n+1)
}

def flatten(xs: List[Any]): List[Any] = xs match {
  case List() => List()
  case (z :: zs) :: ys => flatten(z::zs) ++ flatten(ys)
  case y :: ys => List(y) ++ flatten(ys)
}
