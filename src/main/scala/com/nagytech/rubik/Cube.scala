package com.nagytech.rubik

import com.nagytech.rubik.Face.all

import scala.util.Random


/**
  * As you hold the cube, it has six sides:
  * Front, Back, Top, Bottom, Left, and Right
  *
  * The FB, LR, and TB types help make sure we put the Sides in the right order in functions that ask for them.
  */
sealed trait Side
sealed trait FB // front or back
sealed trait LR // left or right
sealed trait TB // top or bottom
object Top extends Side with TB { override def toString = "Top" }
object Bottom extends Side with TB { override def toString = "Bottom" }
object Left extends Side with LR { override def toString = "Left" }
object Right extends Side with LR { override def toString = "Right" }
object Front extends Side with FB { override def toString = "Front" }
object Back extends Side with FB { override def toString = "Back" }

object Side {
  def sides = Seq(Top, Bottom, Left, Right, Front, Back)

  def opposite(s:Side):Side = s match {
    case Front => Back
    case Back => Front
    case Top => Bottom
    case Bottom => Top
    case Left => Right
    case Right => Left
  }

  /** Choose a side at random. Useful if you want to generate a random turn of a side... */
  def random = sides(Random.nextInt(sides.size))
}

/** A Piece represents a bit of plastic which might have two or three colours on it, depending if it's a corner or an edge piece */
sealed trait Piece

/** A corner piece is on three faces so it has three colours */
case class CornerPiece(c1:Colour, c2:Colour, c3:Colour) extends Piece {
  def isOrientationOf(other:CornerPiece) = Set(c1, c2, c3) == Set(other.c1, other.c2, other.c3)
}

/** An edge piece is on two faces so it has two colours */
case class EdgePiece(c1:Colour, c2:Colour) extends Piece {
  def isOrientationOf(other:EdgePiece) = Set(c1, c2) == Set(other.c1, other.c2)
}

/** A cube has six faces... */
case class Cube(front:Face, back:Face, left:Face, right:Face, top:Face, bottom:Face) {

  /**
    * Because Faces are mutable, we override copy to copy the faces
    */
  def copy(front:Face=front, back:Face=back, left:Face=left, right:Face=right, top:Face=top, bottom:Face=bottom) = {
    Cube(front.copy(), back.copy(), left.copy(), right.copy(), top.copy(), bottom.copy())
  }

  /**
    * Gets the Face on a particular side of the cube
    */
  def face(s:Side):Face = s match {
    case Front => front
    case Back => back
    case Top => top
    case Bottom => bottom
    case Left => left
    case Right => right
  }


  /**
    * If you consider an edge of a side, the "edge piece" (rather than the corner pieces)
    * is the piece in the middle of that edge. It has two colours on it -- one on this side,
    * and one on the adjoining side.
    */
  def edgePiece(s:Side, e:Edge) = {
    val (adjSide, adjEdge) = Cube.adjoins((s, e))
    EdgePiece(
      face(s).edge(e)._2,
      face(adjSide).edge(adjEdge)._2
    )
  }

  /**
    * Returns a sequence of pieces (in clockwise order) on a given edge. There'll be a corner, an edge piece, and a corner
    */
  def piecesOnEdge(s:Side, e:Edge) = (s, e) match {
    case (Top, North) => Seq(cornerPiece(Top, Left, Back), edgePiece(Top, North), cornerPiece(Top, Right, Back))
    case (Top, East) => Seq(cornerPiece(Top, Right, Back), edgePiece(Top, East), cornerPiece(Top, Right, Front))
    case (Top, South) => Seq(cornerPiece(Top, Right, Front), edgePiece(Top, South), cornerPiece(Top, Left, Front))
    case (Top, West) => Seq(cornerPiece(Top, Left, Front), edgePiece(Top, West), cornerPiece(Top, Left, Back))

    case (Front, North) => Seq(cornerPiece(Top, Left, Front), edgePiece(Front, North), cornerPiece(Top, Right, Front))
    case (Front, East) => Seq(cornerPiece(Top, Right, Front), edgePiece(Front, East), cornerPiece(Bottom, Right, Front))
    case (Front, South) => Seq(cornerPiece(Bottom, Right, Front), edgePiece(Front, South), cornerPiece(Bottom, Left, Front))
    case (Front, West) => Seq(cornerPiece(Bottom, Left, Front), edgePiece(Front, West), cornerPiece(Bottom, Left, Front))

    case (Right, North) => Seq(cornerPiece(Top, Right, Front), edgePiece(Right, North), cornerPiece(Top, Right, Back))
    case (Right, East) => Seq(cornerPiece(Top, Right, Back), edgePiece(Right, East), cornerPiece(Bottom, Right, Back))
    case (Right, South) => Seq(cornerPiece(Bottom, Right, Back), edgePiece(Right, South), cornerPiece(Bottom, Right, Front))
    case (Right, West) => Seq(cornerPiece(Bottom, Right, Front), edgePiece(Right, West), cornerPiece(Top, Right, Front))

    case (Back, North) => Seq(cornerPiece(Top, Right, Back), edgePiece(Back, North), cornerPiece(Top, Left, Back))
    case (Back, East) => Seq(cornerPiece(Top, Left, Back), edgePiece(Back, East), cornerPiece(Bottom, Left, Back))
    case (Back, South) => Seq(cornerPiece(Bottom, Left, Back), edgePiece(Back, South), cornerPiece(Bottom, Right, Back))
    case (Back, West) => Seq(cornerPiece(Bottom, Right, Back), edgePiece(Back, West), cornerPiece(Top, Right, Back))

    case (Left, North) => Seq(cornerPiece(Top, Left, Back), edgePiece(Left, North), cornerPiece(Top, Left, Front))
    case (Left, East) => Seq(cornerPiece(Top, Left, Front), edgePiece(Left, East), cornerPiece(Bottom, Left, Front))
    case (Left, South) => Seq(cornerPiece(Bottom, Left, Front), edgePiece(Left, South), cornerPiece(Bottom, Left, Back))
    case (Left, West) => Seq(cornerPiece(Bottom, Left, Back), edgePiece(Left, West), cornerPiece(Top, Left, Back))

    case (Bottom, North) => Seq(cornerPiece(Bottom, Left, Front), edgePiece(Bottom, North), cornerPiece(Bottom, Right, Front))
    case (Bottom, East) => Seq(cornerPiece(Bottom, Right, Front), edgePiece(Bottom, East), cornerPiece(Bottom, Right, Back))
    case (Bottom, South) => Seq(cornerPiece(Bottom, Right, Back), edgePiece(Bottom, South), cornerPiece(Bottom, Left, Back))
    case (Bottom, West) => Seq(cornerPiece(Bottom, Left, Back), edgePiece(Bottom, West), cornerPiece(Bottom, Left, Front))
  }

  /** A cube has eight corner pieces */
  def edgePieces(s:Side) = {
    for {
      edge <- Edge.edges
    } yield edgePiece(s, edge)
  }

  /**
    * Gets a corner piece
    */
  def cornerPiece(tb: TB, lr: LR, fb: FB) = {
    (tb, lr, fb) match {
      case (Top, Left, Back) => CornerPiece(top.nw, left.nw, back.ne)
      case (Top, Right, Back) => CornerPiece(top.ne, right.ne, back.nw)
      case (Top, Left, Front) => CornerPiece(top.sw, left.ne, front.nw)
      case (Top, Right, Front) => CornerPiece(top.se, right.nw, front.ne)
      case (Bottom, Left, Front) => CornerPiece(bottom.nw, left.se, front.sw)
      case (Bottom, Right, Front) => CornerPiece(bottom.ne, right.sw, front.se)
      case (Bottom, Left, Back) => CornerPiece(bottom.sw, left.sw, back.se)
      case (Bottom, Right, Back) => CornerPiece(bottom.se, right.se, back.sw)
    }
  }

  /**
    * Gets the four corner pieces that are on a side of a cube
    */
  def cornerPieces(s:Side):Seq[CornerPiece] = s match {
    case Top => Seq(
      cornerPiece(Top, Left, Back),
      cornerPiece(Top, Right, Back),
      cornerPiece(Top, Left, Front),
      cornerPiece(Top, Right, Front)
    )
    case Bottom => Seq(
      cornerPiece(Bottom, Left, Back),
      cornerPiece(Bottom, Right, Back),
      cornerPiece(Bottom, Left, Front),
      cornerPiece(Bottom, Right, Front)
    )
    case Front => Seq(
      cornerPiece(Bottom, Left, Front),
      cornerPiece(Bottom, Right, Front),
      cornerPiece(Top, Left, Front),
      cornerPiece(Top, Right, Front)
    )
    case Back => Seq(
      cornerPiece(Bottom, Left, Back),
      cornerPiece(Bottom, Right, Back),
      cornerPiece(Top, Left, Back),
      cornerPiece(Top, Right, Back)
    )
    case Left => Seq(
      cornerPiece(Top, Left, Back),
      cornerPiece(Top, Left, Front),
      cornerPiece(Bottom, Left, Back),
      cornerPiece(Bottom, Left, Front)
    )
    case Right => Seq(
      cornerPiece(Top, Right, Back),
      cornerPiece(Top, Right, Front),
      cornerPiece(Bottom, Right, Back),
      cornerPiece(Bottom, Right, Front)
    )
  }

  /**
    * All the corner pieces
    */
  def cornerPieces:Seq[CornerPiece] = cornerPieces(Top).++(cornerPieces(Bottom))

  /**
    * Immutably apply a turn. (ie, returning a copy)
    * When we turn a side, the face on that side will rotate, but the edges of the adjoining sides are also
    * affected.
    *
    * For example, if we turn the right side clockwise, the east edge of the front face will move to be the east
    * edge of the top face, which will move to be the west edge of the back face, which will move to be the east
    * edge of the bottom face, which will move to be the east edge of the front face.
    */
  def apply(op:Op):Cube = op match {
    case t:Turn =>

      // First, consider the face we're rotating -- what it'll look like
      val rotateSide = face(t.side).turn(t.dir)

      // Next, get the adjoining edges
      val ourAdjoins = Seq(
        Cube.adjoins((t.side, North)),
        Cube.adjoins((t.side, East)),
        Cube.adjoins((t.side, South)),
        Cube.adjoins((t.side, West))
      )

      // Next, map out which edge moves where
      val edgeMoves = t.dir match {
        case Clockwise =>
          Map(
            ourAdjoins(0) -> ourAdjoins(1),
            ourAdjoins(1) -> ourAdjoins(2),
            ourAdjoins(2) -> ourAdjoins(3),
            ourAdjoins(3) -> ourAdjoins(0)
          )
        case Anticlockwise =>
          Map(
            ourAdjoins(0) -> ourAdjoins(3),
            ourAdjoins(1) -> ourAdjoins(0),
            ourAdjoins(2) -> ourAdjoins(1),
            ourAdjoins(3) -> ourAdjoins(2)
          )
      }

      // Now work out how those edge moves affect each side
      val movedEdges = for {
        (from, to) <- edgeMoves

        (fromSide, fromEdge) = from
        (toSide, toEdge) = to
      } yield {
        toSide -> face(toSide).setting(toEdge, face(fromSide).edge(fromEdge))
      }

      // Create a map of what each side looks like
      val newMap:Map[Side, Face] = Map(
        // The side we're rotating is rotated as we worked out earlier
        t.side -> rotateSide,

        // The opposite side isn't affected
        Side.opposite(t.side) -> face(Side.opposite(t.side))

        // And the other four sides were worked out by the moved edges
      ) ++ movedEdges

      // Now construct a new cube from our map of sides
      Cube.fromMap(newMap)

    case Alg(turns) => turns.foldLeft(this)(_.apply(_))
  }

  /**
    * Mutably applies the given operation (changing the state of this cube, rather than returning a copy)
    */
  def mutApply(op:Op):Unit = op match {
    case t:Turn => {

      // First rotate the face we're turning
      face(t.side).mutTurn(t.dir)

      // The opposite side is unchanged

      // Next, get the adjoining edges
      val ourAdjoins = Seq(
        Cube.adjoins((t.side, North)),
        Cube.adjoins((t.side, East)),
        Cube.adjoins((t.side, South)),
        Cube.adjoins((t.side, West))
      )

      // Next, move the adjoining edges
      t.dir match {
        case Anticlockwise =>
          val oldEdge0 = face(ourAdjoins(0)._1).edge(ourAdjoins(0)._2)
          face(ourAdjoins(0)._1).mutSet(ourAdjoins(0)._2, face(ourAdjoins(1)._1).edge(ourAdjoins(1)._2))
          face(ourAdjoins(1)._1).mutSet(ourAdjoins(1)._2, face(ourAdjoins(2)._1).edge(ourAdjoins(2)._2))
          face(ourAdjoins(2)._1).mutSet(ourAdjoins(2)._2, face(ourAdjoins(3)._1).edge(ourAdjoins(3)._2))
          face(ourAdjoins(3)._1).mutSet(ourAdjoins(3)._2, oldEdge0)
        case Clockwise =>
          val oldEdge3 = face(ourAdjoins(3)._1).edge(ourAdjoins(3)._2)
          face(ourAdjoins(3)._1).mutSet(ourAdjoins(3)._2, face(ourAdjoins(2)._1).edge(ourAdjoins(2)._2))
          face(ourAdjoins(2)._1).mutSet(ourAdjoins(2)._2, face(ourAdjoins(1)._1).edge(ourAdjoins(1)._2))
          face(ourAdjoins(1)._1).mutSet(ourAdjoins(1)._2, face(ourAdjoins(0)._1).edge(ourAdjoins(0)._2))
          face(ourAdjoins(0)._1).mutSet(ourAdjoins(0)._2, oldEdge3)
      }
    }

    case Alg(turns) =>
      turns.foreach(mutApply)

  }

  /**
    * Apply a sequence of operations. This is done mutably on a copy, for speed's sake
    */
  def applySeq(ops:Seq[Op]) = {
    val c = this.copy()
    for { op <- ops } c.mutApply(op)
    c
  }

  /**
    * Takes a number and a set of allowed moves.
    * It turns the number into a unique seq
    */
  def apply(operationNum:Int, fromMoves:Seq[Op]):Cube = {
    val base = fromMoves.size
    var remainder = operationNum
    val cube = this.copy()
    while (remainder >= base) {
      cube.mutApply(fromMoves(remainder % base))
      if (base > 1) {
        remainder /= base
      } else {
        remainder -= 1
      }
    }
    cube.mutApply(fromMoves(remainder))
    cube
  }

  override def toString = {
    s"""
       |     ${top.nw}${top.nm}${top.ne}
       |     ${top.mw}${top.mm}${top.me}
       |     ${top.sw}${top.sm}${top.se}
       |
       | ${left.nw}${left.nm}${left.ne} ${front.nw}${front.nm}${front.ne} ${right.nw}${right.nm}${right.ne} ${back.nw}${back.nm}${back.ne}
       | ${left.mw}${left.mm}${left.me} ${front.mw}${front.mm}${front.me} ${right.mw}${right.mm}${right.me} ${back.mw}${back.mm}${back.me}
       | ${left.sw}${left.sm}${left.se} ${front.sw}${front.sm}${front.se} ${right.sw}${right.sm}${right.se} ${back.sw}${back.sm}${back.se}
       |
       |     ${bottom.nw}${bottom.nm}${bottom.ne}
       |     ${bottom.mw}${bottom.mm}${bottom.me}
       |     ${bottom.sw}${bottom.sm}${bottom.se}
       |
     """.stripMargin
  }
}

object Cube {

  /**
    * Each face adjoins an edge of four other face.
    * eg, the North edge of the Front face adjoins the South edge of the Top face.
    */
  val adjoins = Map(
    (Front, North) -> (Top, South),
    (Front, East) -> (Right, West),
    (Front, South) -> (Bottom, North),
    (Front, West) -> (Left, East),

    (Right, North) -> (Top, East),
    (Right, East) -> (Back, West),
    (Right, South) -> (Bottom, East),
    (Right, West) -> (Front, East),

    (Back, North) -> (Top, North),
    (Back, East) -> (Left, West),
    (Back, South) -> (Bottom, South),
    (Back, West) -> (Right, East),

    (Left, North) -> (Top, West),
    (Left, East) -> (Front, West),
    (Left, South) -> (Bottom, West),
    (Left, West) -> (Back, East),

    (Bottom, North) -> (Front, South),
    (Bottom, East) -> (Right, South),
    (Bottom, South) -> (Back, South),
    (Bottom, West) -> (Left, South),

    (Top, North) -> (Back, North),
    (Top, East) -> (Right, North),
    (Top, South) -> (Front, North),
    (Top, West) -> (Left, North)
  )


  /**
    * A fresh new cube, not scrambled.
    */
  def fresh = {
    Cube(
      top = all(W),
      front = all(G),
      right = all(R),
      left = all(O),
      back = all(B),
      bottom = all(Y)
    )
  }

  /**
    * Turns a map of Side -> Face into a Cube
    */
  def fromMap(m:Map[Side, Face]) = {
    Cube(front=m(Front), back=m(Back), left=m(Left), right=m(Right), top=m(Top), bottom=m(Bottom))
  }

}




