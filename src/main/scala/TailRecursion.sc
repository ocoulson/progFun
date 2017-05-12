def factorial(x: Int): Int = {
  if (x == 0) 1
  else x * factorial(x-1)
}

factorial(6)

def factTail(x: Int, total: Int): Int = {
  if (x == 0) total
  else factTail(x-1, total * x)
}

factTail(6, 1)
