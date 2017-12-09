package constraints

import cafesat.api.FormulaBuilder._
import cafesat.api.Formulas._
import cafesat.api.Solver._

import scala.annotation.tailrec

/**
  * This object contains utility functions for encoding
  * some arithmetic constraints as boolean ones
  */
object Arithmetic {

  /**
   * Transforms a positive integer in binary form into its integer representation.
   * The `head` element of the input list contains the most
   * significant bit (the list is in big-endian form).
   */
  def binary2int(n: List[Boolean]): Int = {
    if (n.isEmpty) 0
    else if (n.head) Math.pow(2,n.length-1).asInstanceOf[Int] + binary2int(n.tail)
    else binary2int(n.tail)
  }

  /**
   * Encodes a positive integer number into base 2.
   * The `head` element of the resulting list contains the most significant
   * bit. This function should not return unnecessary leading zeros.
   */
  def int2binary(n: Int): List[Boolean] = {
    if (n==0) List(false) else {
      def aux(x: Int, acc: List[Boolean]): List[Boolean] = x match {
        case 0 => acc
        case x => if (x % 2 == 0) aux(x / 2, false :: acc) else aux((x - 1) / 2, true :: acc)
      }
      aux(n, List())
    }
  }

  /**
   * This function takes two arguments, both representing positive
   * integers encoded in binary as lists of propositional formulas
   * (true for 1, false for 0). It returns
   * a formula that represents a boolean circuit that constraints
   * `n1` to be less than or equal to `n2`
   */
  def lessEquals(n1: List[Formula], n2: List[Formula]): Formula = {
    def lessOrEquals(liste:List[(Formula,Formula)], acc: Formula): Formula = liste match{
      case Nil => acc
      case x::xs => x match{
        case (a,b) => lessOrEquals(xs, (!a && b) || (a iff b) && acc)
      }
    }
    val fal: Formula = false
    lessOrEquals(n1.reverse.zipAll(n2.reverse,fal, fal), true)


  }

  /**
   * A full adder is a circuit that takes 3 one bit numbers, and returns the
   * result encoded over two bits: (cOut, s)
   */
  def fullAdder(a: Formula, b: Formula, cIn: Formula): (Formula, Formula) = {
    def xor(c: Formula,d: Formula): Formula = (!c && d) || (c && !d)
    val s = xor(xor(a,b),cIn)
    val c = (xor(a,b) && cIn)||(a&&b)
    (c,s)
  }

  /**
   * This function takes two arguments, both representing positive integers
   * encoded as lists of propositional variables. It returns a pair.
   *
   * The first element of the pair is a `List[Formula]`, and it represents
   * the resulting binary number.
   * The second element is a set of intermediate constraints that are created
   * along the way.
   *
   */
  def adder(n1: List[Formula], n2: List[Formula]): (List[Formula], Set[Formula]) = {
    val c0 = false
    def makeSameLength(x:Int,y: List[Formula]): List[Formula] = x match{
      case 0 => y
      case k => makeSameLength(k-1,false::y)
    }
    //1. Avoir le meme nombre de bits
    val (list1,list2) = {
      if (n1.length == n2.length) (n1,n2)
      else if (n1.length > n2.length) (n1,makeSameLength(n1.length-n2.length,n2))
      else (makeSameLength(n2.length-n1.length,n1),n2)
    }

    def add(x1: List[Formula], x2: List[Formula]):(List[Formula],Set[Formula])= (x1,x2) match{
      case(x::Nil,y::Nil) => {
        val cOut: PropVar = propVar("cOut")
        val s: PropVar = propVar("s")
        (List(s,cOut),Set(s iff fullAdder(x,y,c0)._1,cOut iff fullAdder(x,y,c0)._2))
      }
      case (x::xs,y::ys) =>{
      val cOut: PropVar = propVar("cOut")
      val s: PropVar = propVar("s")
      val (l::ls,setCon) = add(xs,ys)

        (s::cOut::ls,setCon.+(cOut iff fullAdder(x,y,l)._2,s iff fullAdder(x,y,l)._1))
      }
    }
    add(list1,list2)
  }

  /**
   * A helper function that creates a less-equals formula
   * taking an integer and a formula as parameters
   */
  def lessEqualsConst(cst: Int, n: List[Formula]): Formula = {
    lessEquals(int2binary(cst), n)
  }

  /**
   * A helper function that creates a less-equals formula
   * taking a formula and an integer as parameters
   */
  def lessEqualsConst(n: List[Formula], cst: Int): Formula = {
    lessEquals(n, int2binary(cst))
  }


}
