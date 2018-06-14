package com.nagytech.rubik

import com.nagytech.rubik.Solver._
import org.scalatest._

class RubikSpec extends FlatSpec with Matchers {

  "Alg R R'" should "cancel out" in {
    val c = Cube.fresh
    c.mutApply(Alg(Seq(Turn.R, Turn.Ra)))
    c should be (Cube.fresh)bill
  }

  "Solver.all" should "and together the various conditions" in {
    val truthy:CubeTest = _ => true
    val falsey:CubeTest = _ => false

    Solver.all(Seq(truthy, truthy))(Cube.fresh) should be (true)
    Solver.all(Seq(truthy, truthy, truthy, truthy))(Cube.fresh) should be (true)
    Solver.all(Seq(truthy, truthy, falsey, truthy))(Cube.fresh) should be (false)
  }

  "Solver.any" should "or together the various conditions" in {
    val truthy:CubeTest = _ => true
    val falsey:CubeTest = _ => false

    Solver.any(Seq(truthy, truthy))(Cube.fresh) should be (true)
    Solver.any(Seq(truthy, truthy, truthy, truthy))(Cube.fresh) should be (true)
    Solver.any(Seq(truthy, truthy, falsey, truthy))(Cube.fresh) should be (true)
    Solver.any(Seq(falsey, truthy, falsey, falsey))(Cube.fresh) should be (true)
    Solver.any(Seq(falsey, falsey, falsey, falsey))(Cube.fresh) should be (false)
  }

  "Solver.search" should "find the inverse of a very short sequence" in {
    for {
      alg <- Seq(
        Alg(Seq(Turn.Ra)),
        Alg(Seq(Turn.R, Turn.U)),
        Alg(Seq(Turn.R, Turn.U, Turn.D))
      )
    } {
      Solver.search(Cube.fresh.apply(alg), 50000, _ == Cube.fresh, 0, Turn.allTurns).map(_.alg) should be (Some(alg.inverse))
    }
  }

  "Solver.edgePieceInPlace" should "check edge piece has same colours, but may be in different orientation" in {
    val c = Cube.fresh
    c.face(Top).sm = G
    c.face(Front).nm = W
    c.face(Left).me = W

    edgePieceInPlace(Top, South)(c) should be (true)
    edgePieceInPlace(Front, North)(c) should be (true)
    edgePieceInPlace(Top, North)(c) should be (true)
    edgePieceInPlace(Left, East)(c) should be (false)
  }

  "Solver.edgePieceCorrect" should "check edge piece has same colours, and is in same orientation" in {
    val c = Cube.fresh
    c.face(Top).sm = G
    c.face(Front).nm = W
    c.face(Left).me = W

    edgePieceCorrect(Top, South)(c) should be (false)
    edgePieceCorrect(Front, North)(c) should be (false)
    edgePieceCorrect(Top, North)(c) should be (true)
    edgePieceCorrect(Left, East)(c) should be (false)
  }


  "Solver.cornerPieceInPlace" should "check corner piece has same colours, but may be in different orientation" in {
    val c = Cube.fresh
    c.face(Top).sw = G
    c.face(Front).nw = W

    c.face(Left).se = W

    cornerPieceInPlace(Top, Left, Front)(c) should be (true)
    cornerPieceInPlace(Top, Right, Front)(c) should be (true)
    cornerPieceInPlace(Bottom, Left, Front)(c) should be (false)
  }

  "Solver.cornerPieceCorrect" should "check corner piece has same colours, and is in same orientation" in {
    val c = Cube.fresh
    c.face(Top).sw = G
    c.face(Front).nw = W

    c.face(Left).se = W

    cornerPieceCorrect(Top, Left, Front)(c) should be (false)
    cornerPieceCorrect(Top, Right, Front)(c) should be (true)
    cornerPieceCorrect(Bottom, Left, Front)(c) should be (false)
  }

  "Solver.whiteCrossCorrect" should "check the four white edge squares are correct on the top face" in {
    // All white top face!
    whiteCrossCorrect(Cube.fresh) should be (true)

    // One edge green!
    whiteCrossCorrect(Cube.fresh.apply(Turn.R)) should be (false)

    // The cross is still white, even though there's a corner square of a different colour
    whiteCrossCorrect(Cube.fresh.applySeq(Seq(Turn.R, Turn.D, Turn.Ra))) should be (true)
  }

  "Solver.whiteCornerUnder" should "check the if a white corner is vertically underneath where it should sit" in {
    // No, it's in place
    whiteCornerUnder(Right, Front)(Cube.fresh) should be (false)

    // Under
    whiteCornerUnder(Right, Front)(Cube.fresh.apply(Turn.Ra)) should be (true)

    // Under
    whiteCornerUnder(Left, Front)(Cube.fresh.apply(Turn.Fa)) should be (true)
  }

  "Solver.topCorrect" should "check the top layer has been solved" in {
    topCorrect(Cube.fresh) should be (true)
    topCorrect(Cube.fresh.apply(Turn.D)) should be (true)  // doesn't affect the top layer
    topCorrect(Cube.fresh.apply(Turn.R)) should be (false)
    topCorrect(Cube.fresh.applySeq(Seq(Turn.R, Turn.D, Turn.Ra))) should be (false)
  }

  "Solver.solveTop" should "solve the top layer" in {
    solveTop(Cube.fresh.apply(12313, Turn.allTurns)).map({ t => topCorrect(t.after)}) should be (Some(true))
  }

  "Solver.topMiddleCorrect" should "check the top and middle layers have been solved" in {
    topMiddleCorrect(Cube.fresh) should be (true)
    topMiddleCorrect(Cube.fresh.apply(Turn.D)) should be (true)  // doesn't affect the top layer
    topMiddleCorrect(Cube.fresh.apply(Turn.R)) should be (false)
    topMiddleCorrect(Cube.fresh.applySeq(Seq(Turn.R, Turn.D, Turn.Ra))) should be (false)
  }

  "Solver.yellowCross" should "check whether bottom nm me sm and mw are yellow" in {
    yellowCross(Cube.fresh) should be (true)
    yellowCross(Cube.fresh.apply(Turn.U)) should be (true)
    yellowCross(Cube.fresh.apply(Turn.D)) should be (true) // still yellow, even though they've now moved around
    yellowCross(Cube.fresh.apply(Turn.R)) should be (false)
  }

}
