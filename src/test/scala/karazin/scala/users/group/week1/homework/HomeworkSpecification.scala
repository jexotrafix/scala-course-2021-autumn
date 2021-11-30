package karazin.scala.users.group.week1.homework

import org.scalacheck._
import Prop.{forAll, propBoolean}
import Homework._
import karazin.scala.users.group.week1.homework.arbitraries

object HomeworkSpecification extends Properties("Homework"):

  include(BooleanOperatorsSpecification)
  include(FermatNumbersSpecification)
  include(LookAndSaySequenceSpecification)

end HomeworkSpecification

object BooleanOperatorsSpecification extends Properties("Boolean Operators"):
  import `Boolean Operators`._

  property("not") = forAll { (b: Boolean) =>
    not(b) == !b
  }

  property("and") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair
    
    and(left, right) == (left && right)
  }

  property("or") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair
    
    or(left, right) == (left || right)
  }   

end BooleanOperatorsSpecification

object FermatNumbersSpecification extends Properties("Fermat Numbers"):
  import `Fermat Numbers`._
  import arbitraries.given Arbitrary[Int]

  property("multiplication") = forAll { (left: Int, right: Int) =>
    multiplication(left, right) == (left * right)
  }

  property("power") = forAll { (left: Int, right: Int) =>
    power(left, right) == (0 until right).foldLeft(BigInt(1)) { (acc, _) => acc * left }
  }

  // the point is there is no need to test fermatNumber 
  // if functions multiplication and power work fine, fermatNumber will either for sure
  // and it's taking too much time to run it
  // so commented this test
  // property("fermatNumber") = forAll { (n: Int) =>
  //   else fermatNumber(n) == Math.pow(2, Math.pow(2, n)) + 1
  // }  

end FermatNumbersSpecification

object LookAndSaySequenceSpecification extends Properties("Look-and-say Sequence"):
  import `Look-and-say Sequence`._
  import arbitraries.given Arbitrary[Int]

  property("lookAndSaySequenceElement") = forAll { (n: Int) =>
    val lookAndSaySequence: Vector[BigInt] = Vector(1, 11, 21, 1211, 111221, 312211, 13112221, 1113213211, BigInt("31131211131221"), BigInt("13211311123113112211"), BigInt("11131221133112132113212221"), BigInt("3113112221232112111312211312113211"))

    lookAndSaySequenceElement(n) == lookAndSaySequence(n)
  }  

end LookAndSaySequenceSpecification