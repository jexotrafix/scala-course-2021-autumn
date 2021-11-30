package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec
/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework :

  object `Boolean Operators` :

    def not(b: Boolean): Boolean = b match {
      case true => false
      case _ => true
    }

    def and(left: Boolean, right: Boolean): Boolean = (left, right) match {
      case (true, true) => true
      case _ => false
    }

    def or(left: Boolean, right: Boolean): Boolean = (left, right) match {
      case (false, false) => false
      case _ => true
    }

  end `Boolean Operators`

  object `Fermat Numbers` :

    val multiplication: (BigInt, BigInt) => BigInt = (a, b) => {
      @tailrec
      def inner(a: BigInt, b: BigInt, acc: BigInt): BigInt = a match {
        case 0 => acc
        case _ => inner(a - 1, b, acc + b)
      }
  
      // to reduce an amount of inner function calls
      inner(a min b, a max b, 0)
    }

    val power: (BigInt, BigInt) => BigInt = (a, b) => {
      @tailrec
      def inner(a: BigInt, b: BigInt, acc: BigInt): BigInt = b match {
        case 0 => acc
        case _ => inner(a, b - 1, multiplication(acc, a))
      }
  
      inner(a, b, 1)
    }

    val fermatNumber: Int => BigInt = n => power(2, power(2, n)) + 1

  end `Fermat Numbers`

  object `Look-and-say Sequence` :

    def toLookAndSay(n: BigInt): BigInt = {
      // converting n to list for convenience
      val nList = n.toString.map(_.asDigit).toList

      @tailrec
      def inner(ints: List[Int], cur: BigInt, cnt: Int, acc: BigInt): BigInt = ints match {
        case List() => (acc * 10 + cnt) * 10 + cur
        case h :: t => {
          if (h == cur) inner(t, cur, cnt + 1, acc)
          else inner(t, h, 1, (acc * 10 + cnt) * 10 + cur)
        }
      }
  
      inner(nList.tail, nList.head, 1, 0)
    }

    val lookAndSaySequenceElement: Int => BigInt = n => {
        @tailrec 
        def loop(cur: BigInt, left: BigInt): BigInt = left match {
          case 0 => cur
          case _ => loop(toLookAndSay(cur), left - 1)
        }

        loop(1, n)
    }

  end `Look-and-say Sequence`

end Homework