def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  def iteration(current: Int) : Boolean = {
    if (current == 1) ordered(as(current), as(current - 1 ))
    else if (!ordered(as(current), as(current - 1))) false
    else iteration(current -1)
  }
  iteration(as.length)
}

println(isSorted(Array(1,2,3), (x: Int, y:Int) => x > y))