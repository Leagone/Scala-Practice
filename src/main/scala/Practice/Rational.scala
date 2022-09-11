package Practice

case class Rational(x: Int, y: Int) {

  private def gcd(num: Int, denm: Int): Int = {
    if (denm == 0) num else gcd(denm, num % denm)
  }

  def numer = x / gcd(x.abs, y)

  def denom = y / gcd(x.abs, y)

  def add(r: Rational): Rational = {
    Rational(numer * r.denom + denom * r.numer, denom * r.denom)
  }

  def substrate(r: Rational): Rational = {
    Rational(numer * r.denom - denom * r.numer, denom * r.denom)
  }

  def multiply(r: Rational): Rational = {
    Rational(numer * r.numer, denom * r.denom)
  }

  def div(r: Rational): Rational = {
    Rational(numer * r.denom, denom * r.numer)
  }

  def neg(): Rational = {
    Rational(-numer, -denom)
  }

}