def pascal(c: Int, r: Int): Int = {
  if(c <= 0 || c == r) 1
  else pascal(c, r-1) + pascal(c-1, r-1)
}

pascal(0,0)
pascal(2,2)
pascal(3,3)


pascal(1,2)
pascal(1,3)
pascal(2,4)


def balance(chars: List[Char]): Boolean = {
  def helper(characters: List[Char], count: Int): Boolean = {
    if (characters.isEmpty) count == 0
    else {
      if (count < 0) false
      else characters.head match {
        case '(' => helper(characters.tail,count+1)
        case ')' => helper(characters.tail,count-1)
        case _ => helper(characters.tail, count)
      }
    }
  }
  helper(chars, 0)
}

balance("()".toList)
balance(")(".toList)
balance("()(())".toList)
balance("()(()".toList)

