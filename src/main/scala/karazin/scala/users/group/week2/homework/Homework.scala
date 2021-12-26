package karazin.scala.users.group.week2.homework

import scala.annotation.targetName
import scala.math.{abs, signum}

object Homework:

  // `x` and `y` are inaccessible from outside
  class Rational(x: Int, y: Int):
    // Checking the precondition. Is fails then throws `IllegalArgumentException`
    require(y > 0, "Denominator must be positive")

    def this(x: Int) = this(x, 1)

    val numer = x / g
    val denom = y / g

    // Defines an external name for a definition
    @targetName("less than")
    // Annotation on a method definition allows using the method as an infix operation
    infix def <(that: Rational): Boolean =
      this.numer * that.denom < that.numer * this.denom

    @targetName("less or equal")
    infix def <=(that: Rational): Boolean =
      this < that || this == that

    @targetName("greater than")
    infix def >(that: Rational): Boolean =
      !(this <= that)

    @targetName("greater or equal")
    infix def >=(that: Rational): Boolean =
      !(this < that)

    @targetName("addition")
    infix def +(that: Rational): Rational = {
      val l = lcd(this.denom, that.denom)

      Rational(this.numer * (l / this.denom) + that.numer * (l / that.denom), l)
    }

    @targetName("negation")
    infix def unary_- : Rational = 
      Rational(-this.numer, this.denom)

    @targetName("substraction")
    infix def -(that: Rational): Rational = 
      this + (-that)

    @targetName("multiplication")
    infix def *(that: Rational): Rational = 
      Rational(this.numer * that.numer, this.denom * that.denom)

    @targetName("division")
    infix def /(that: Rational): Rational = {
      require(that.numer != 0, "Division by zero will be caused")

      val resNumer = this.numer * that.denom
      val resDenom = this.denom * that.numer

      if (resDenom < 0) Rational(-resNumer, abs(resDenom))
      else Rational(resNumer, resDenom)
    }

    override def toString: String = s"${this.numer}/${this.denom}"

    def toDouble: Double = this.numer.toDouble / this.denom

    private def gcd(a: Int, b: Int): Int =
      if b == 0 then a else gcd(b, a % b)

    private lazy val g = gcd(abs(x), y)

    // lcd for denominators which are always greater than zero
    // so no need to use abs()
    private def lcd(a: Int, b: Int): Int = 
      (a * b) / gcd(a, b)

    override def equals(other: Any): Boolean = other match {
      case rational: Rational => (this.numer == rational.numer) && (this.denom == rational.denom)
      case _                  => false
    }

    override def hashCode: Int = {
      val prime = 61
      prime * (prime + numer.hashCode) + denom.hashCode
    }
    
  end Rational

end Homework