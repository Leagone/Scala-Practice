def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
  (A) => (B) => f(A,B)
}
def uncurry[A,B,C](f: A => B => C) : (A, B) => C = {
  (A,B) => f(A)(B)
}