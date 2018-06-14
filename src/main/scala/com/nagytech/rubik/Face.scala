package com.nagytech.rubik

/**
  * A face has nine squares, each of which can be a different colour.
  *
  * nw  nm  ne
  * mw  mm  me
  * sw  sm  se
  *
  * A trick with the cube is to remember that the mm square on a face never changes colour.
  * (If you rotate this face, you can turn it in-place, but there's no turn you can make that will move mm to a different face)
  *
  * Faces are mutable for efficiency's sake -- we will find ourselves wanting to do (literally) millions of operations
  * on Faces in a small space of time, so getting rid of the object allocation time ends up giving about a five-fold
  * speed improvement.
  *
  * There are a set of mutating and a set of copying operations.
  */
case class Face(
  var nw:Colour, var nm:Colour, var ne:Colour,
  var mw:Colour, var mm:Colour, var me:Colour,
  var sw:Colour, var sm:Colour, var se:Colour
) {

  /**
    * The squares as a sequence in a fixed order.
    * This makes it easier to do things like "count how many squares are the wrong colour"
    */
  def squares = Seq(nw, nm, ne, mw, mm, me, sw, sm, se)

  /**
    * Note, these are returned in a clockwise order, to make it easy to work out how edges change when a side
    * is rotated.
    */
  def edge(e:Edge):(Colour, Colour, Colour) = e match {
    case North => (nw, nm, ne)
    case East => (ne, me, se)
    case South => (se, sm, sw)
    case West => (sw, mw, nw)
  }

  /**
    * Copies this face, setting the colours on one edge.
    * Again, these are done in a clockwise order, to make it easy to work out how edges change when a side
    * is rotated.
    */
  def setting(e:Edge, c:(Colour, Colour, Colour)):Face = e match {
    case North => copy(nw=c._1, nm=c._2, ne=c._3)
    case East => copy(ne=c._1, me=c._2, se=c._3)
    case South => copy(se=c._1, sm=c._2, sw=c._3)
    case West => copy(sw=c._1, mw=c._2, nw=c._3)
  }

  /**
    * Mutate this Face by setting the edge to these colours
    */
  def mutSet(e:Edge, c:(Colour, Colour, Colour)):Unit = e match {
    case North =>
      nw=c._1
      nm=c._2
      ne=c._3
    case East =>
      ne=c._1
      me=c._2
      se=c._3
    case South =>
      se=c._1
      sm=c._2
      sw=c._3
    case West =>
      sw=c._1
      mw=c._2
      nw=c._3
  }

  /**
    * What happens if you turn this face?
    */
  def turn(t:Direction):Face = t match {
    case Clockwise =>     Face(sw, mw, nw, sm, mm, nm, se, me, ne)
    case Anticlockwise => Face(
      ne, me, se,
      nm, mm, sm,
      nw, mw, sw)
  }

  /**
    * Mutates this Face by turning it
    */
  def mutTurn(t:Direction):Unit = t match {
    case Clockwise =>
      // cycle corners
      val onw = nw
      nw = sw
      sw = se
      se = ne
      ne = onw

      // cycle edges
      val omw = mw
      mw = sm
      sm = me
      me = nm
      nm = omw
    case Anticlockwise =>
      // cycle corners
      val onw = nw
      nw = ne
      ne = se
      se = sw
      sw = onw

      // cycle edges
      val omw = mw
      mw = nm
      nm = me
      me = sm
      sm = omw
  }
}

object Face {
  /**
    * Create a face where every square is the same colour
    * eg, Face.all(W)
    */
  def all(c:Colour) = Face(c,c,c,c,c,c,c,c,c)
}
