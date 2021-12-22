package karazin.scala.users.group.week3

import scala.annotation.tailrec

object Homework:
  
  // Peano numbers
  abstract class Nat:
    def isZero: Boolean
    def predecessor: Nat
    
    infix def + (that: Nat): Nat
    
    infix def - (that: Nat): Nat
    
    // Optional task
    def toInt: Int
    
    // Optional task
    def fromInt(int: Int): Nat = {
      if (int < 0) throw new Exception("Breaking the closeness of natural numbers")

      @tailrec
      def inner(int: Int, acc: Nat): Nat = int match {
        case 0 => acc
        case _ => inner(int - 1, Succ(acc))
      }

      inner(int, Zero)
    }
  
    override def toString: String = s"Succ($predecessor)"
  
  type Zero = Zero.type 
  object Zero extends Nat:
    def isZero: Boolean = true
    def predecessor: Nat = throw new Exception("0 doesn't have a predecessor")
    
    infix def +(that: Nat): Nat = that
    
    infix def -(that: Nat): Nat = that match {
      case z: Zero => this
      case _       => throw new Exception("Breaking the closeness of natural numbers")
    } 
    
    // Optional task
    def toInt: Int = 0

    override def toString: String = "Zero"
    override def equals(obj: Any): Boolean = obj match {
      case z: Zero        => true
      case _              => false
    }

  class Succ(n: Nat) extends Nat:
    def isZero: Boolean = false
    def predecessor: Nat = n
    
    infix def +(that: Nat): Nat = {
      @tailrec
      def inner(nat: Nat, acc: Nat): Nat = nat match {
        case z: Zero => acc
        case _       => inner(nat.predecessor, Succ(acc))
      }

      inner(that, this)
    }
    
    infix def -(that: Nat): Nat = {
      @tailrec
      def inner(nat: Nat, acc: Nat): Nat = nat match {
        case z: Zero => acc
        case _       => inner(nat.predecessor, acc.predecessor)
      }

      inner(that, this)
    }
    
    // Optional task
    def toInt: Int = {
      @tailrec
      def inner(n: Nat, acc: Int): Int = n match {
        case z: Zero => acc
        case _       => inner(n.predecessor, acc + 1)
      }

      inner(this, 0)
    }

    override def equals(obj: Any): Boolean = obj match {
      case succ: Succ     => this.predecessor == succ.predecessor
      case _              => false 
    }

