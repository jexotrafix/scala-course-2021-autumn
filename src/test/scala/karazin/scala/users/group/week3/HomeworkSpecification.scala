package karazin.scala.users.group.week3

import scala.math._
import org.scalacheck._
import Prop.{forAll, propBoolean, throws}
import Homework._

import arbitraries.{given Arbitrary[Nat], given Arbitrary[Zero], given Arbitrary[Succ], given Arbitrary[Int]}

object ZeroSpecification extends Properties("Zero"):
  property("is zero cheeeck") = forAll { (z: Zero) =>
    z.isZero
  }

  property("zero predecessor cheeeck") = forAll { (z: Zero) =>
    throws(classOf[Exception]) {
      z.predecessor
    }
  }

  property("equals cheeeck") = forAll { (z: Zero, n: Nat) =>
    (z equals n) == n.isInstanceOf[Zero]
  }

  property("plus operator cheeeck") = forAll { (z: Zero, n: Nat) =>
    (z + n) equals n
  }

  property("minus operator cheeeck") = forAll { (z: Zero, n: Nat) =>
    if (n.isInstanceOf[Zero]) (z - n) equals Zero
    else throws(classOf[Exception]) {
      z - n
    }
  }

  property("to int cheeeck") = forAll { (z: Zero) =>
    z.toInt == 0
  }

end ZeroSpecification


object SuccSpecification extends Properties("Succ"):
  property("is zero cheeeck") = forAll { (s: Succ) =>
    s.isZero == false
  }

  property("succ predecessor cheeeck") = forAll { (n: Nat) =>
    Succ(n).predecessor == n
  }

  property("succ equals cheeeck") = forAll { (s1: Succ, s2: Succ) =>
    s1.equals(s2) == s1.predecessor.equals(s2.predecessor)
  }

  property("to int cheeeck") = forAll { (i: Int, n: Succ) =>
    n.fromInt(i).toInt == i
  }

  property("plus operator cheeeck") = forAll { (s1: Succ, s2: Succ) =>
    (s1 + s2).toInt == (s1.toInt + s2.toInt) 
  }

  property("minus operator cheeeck") = forAll { (i1: Int, i2: Int, n: Nat) =>
    if (i1 < i2) throws(classOf[Exception]) {
      n.fromInt(i1) - n.fromInt(i2)
    }
    else (n.fromInt(i1) - n.fromInt(i2)).toInt == i1 - i2
  } 

end SuccSpecification

object NatSpecification extends Properties("Nat"):
 
  property("from int cheeeck") = forAll { (i: Int, n: Succ) =>
    (n.toInt == i) == n.fromInt(i).equals(n)
  }

  property("to string cheeeck") = forAll { (i: Int, n: Succ) =>
    n.fromInt(i).toString == ("Succ(" * i + "Zero" + ")" * i)
  }

end NatSpecification
  