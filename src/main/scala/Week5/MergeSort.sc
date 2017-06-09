def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (List(), ys1) => ys1
        case (xs1, List()) => xs1
        case (x::xs1, y::ys1) => if (ord.lt(x,y)) x::merge(xs1,ys) else y::merge(xs,ys1)
      }
    val (a,b) = xs.splitAt(n)
    merge(msort(a),msort(b))
  }
}

val nums = List(2,5,-19,23,41,1)
val fruits = List("apple", "banana", "pear", "orange","mango")

msort(nums)

msort(fruits) 