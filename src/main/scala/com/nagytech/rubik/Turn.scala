package com.nagytech.rubik

import scala.collection.mutable

/**
  * Which way we can turn a Face (clockwise or anticlockwise)
  */
sealed trait Direction
case object Clockwise extends Direction
case object Anticlockwise extends Direction

object Direction {
  val directions = Seq(Clockwise, Anticlockwise)

  /** Chooses a direction at random */
  def random = if (scala.util.Random.nextBoolean()) Clockwise else Anticlockwise
}

/**
 * An operation we can apply to the cube
 */
trait Op {
  def inverse:Op

  def apply(cube:Cube):Cube

  /**
    * Produces a short string that's easy to print on a single line
    */
  def short:String

  /**
    * "Rotates" this operation. eg, if the operation was designed to do something on the Front face, this produces a
    * version of the operation that works on the Right face.
    */
  def rotateClockwise:Op

  /**
    * "Rotates" this operation downwards. eg, if the operation was designed to do something on the Front face, this produces a
    * version of the operation that works on the Bottom face.
    */
  def rotateDown:Op

  /**
    * Counts the number of individual quarter-turns in the algorithm
    */
  def countTurns:Int
}



/**
  * A simple turn of a side, one quarter rotation in a direction
  */
case class Turn(side:Side, dir:Direction) extends Op {

  def countTurns = 1

  override def toString = s"Turn $side $dir"

  def inverse = dir match {
    case Clockwise =>      Turn(side, Anticlockwise)
    case Anticlockwise =>  Turn(side, Clockwise)
  }

  def apply(cube:Cube) = cube.apply(this)

  /*
   * We generate the combinations of moves using counting!
   */
  def toNum = Turn.allTurns.indexOf(this)

  def short = {
    (side match {
      case Top => "U"
      case Bottom => "D"
      case Left => "L"
      case Right => "R"
      case Front => "F"
      case Back => "B"
    }) ++ (dir match {
      case Clockwise => " "
      case Anticlockwise => "' "
    })
  }

  def rotateClockwise = side match {
    case Top => this
    case Bottom => this
    case Left => Turn(Front, dir)
    case Front => Turn(Right, dir)
    case Right => Turn(Back, dir)
    case Back => Turn(Left, dir)
  }

  def rotateDown = side match {
    case Top => Turn(Front, dir)
    case Front => Turn(Bottom, dir)
    case Bottom => Turn(Back, dir)
    case Back => Turn(Top, dir)
    case Left => this
    case Right => this
  }
}


object Turn {

  /** a random turn */
  def random = Turn(Side.random, Direction.random)

  /**
    * A random sequence of turns
    */
  def randomSeq(int:Int) = for (i <- 0.until(int)) yield random

  /**
    * All combinations of turns (on each face, clockwise and anticlockwise)
    */
  val allTurns = for {
    side <- Side.sides
    turn <- Direction.directions
  } yield Turn(side, turn)

  /**
    * The inverse of a turn
    */
  def inverse(seq:Seq[Turn]) = for {
    op <- seq.reverse
  } yield op.inverse


  /**
    * Changes a number into a sequence of digits in the given base. Used for generating Transforms from numbers.
    */
  def toBase(num:Int, base:Int):Seq[Int] = {
    val buf = mutable.Buffer.empty[Int]
    var remainder = num
    while (remainder >= base) {
      buf.append(remainder % base)
      if (base > 1) {
        remainder /= base
      } else {
        remainder -= 1
      }
    }
    buf.append(remainder)
    buf.reverse.toSeq
  }

  def fromBase(s:Seq[Int], base:Int):Int = {
    s.foldLeft(0)((soFar, digit) => soFar * base + digit)
  }

  /**
    * This is designed to return a Transform with the same moves that Cube.apply(i, fromMoves) would perform
    */
  def fromNum(i:Int, fromMoves:Seq[Op]) = {
    toBase(i, fromMoves.size).map(fromMoves.apply).reverse
  }

  def toInt(s:Seq[Turn]) = if (s.isEmpty) -1 else {
    fromBase(s.map(allTurns.indexOf), allTurns.size)
  }

  def main(args:String*) {
    val c = Cube.fresh
    println(c)
  }

  /** shorthand for a quarter turn of the right face, clockwise */
  val R = Turn(Right, Clockwise)
  /** shorthand for a quarter turn of the right face, anticlockwise */
  val Ra = Turn(Right, Anticlockwise)
  /** shorthand for a quarter turn of the left face, clockwise */
  val L = Turn(Left, Clockwise)
  /** shorthand for a quarter turn of the left face, anticlockwise */
  val La = Turn(Left, Anticlockwise)
  /** shorthand for a quarter turn of the top face, clockwise */
  val U = Turn(Top, Clockwise)
  /** shorthand for a quarter turn of the top face, anticlockwise */
  val Ua = Turn(Top, Anticlockwise)
  /** shorthand for a quarter turn of the bottom face, clockwise */
  val D = Turn(Bottom, Clockwise)
  /** shorthand for a quarter turn of the bottom face, anticlockwise */
  val Da = Turn(Bottom, Anticlockwise)
  /** shorthand for a quarter turn of the front face, clockwise */
  val F = Turn(Front, Clockwise)
  /** shorthand for a quarter turn of the front face, anticlockwise */
  val Fa = Turn(Front, Anticlockwise)
  /** shorthand for a quarter turn of the back face, clockwise */
  val B = Turn(Back, Clockwise)
  /** shorthand for a quarter turn of the back face, anticlockwise */
  val Ba = Turn(Back, Anticlockwise)

}

/**
  * An Alg (algorithm) is a remembered sequence of moves.
  *
  * We'll find ourselves using these rather a lot.
  */
case class Alg(turns:Seq[Op]) extends Op {

  /**
    * To inverse a sequence of turns, reverse the sequence and inverse the turns
    *
    * @return
    */
  def inverse = Alg(turns.reverse.map {
    case Turn(side, Clockwise) =>      Turn(side, Anticlockwise)
    case Turn(side, Anticlockwise) =>  Turn(side, Clockwise)
  })

  def apply(cube:Cube) = cube.apply(this)

  def short = {
    val sb = new mutable.StringBuilder("(")
    for (t <- turns) {
      sb.append(t.short)
      sb.append(" ")
    }
    sb.append(")")
    sb.toString
  }


  def rotateClockwise = Alg(turns.map(_.rotateClockwise))

  def rotateDown = Alg(turns.map(_.rotateDown))

  def countTurns = turns.map(_.countTurns).sum
}
