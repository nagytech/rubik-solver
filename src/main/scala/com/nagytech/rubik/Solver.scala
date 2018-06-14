package com.nagytech.rubik



import scala.annotation.tailrec
import scala.collection.mutable

/**
  * A Transform is what each stage of the Solver returns -- it contains a sequence of operations, and also
  * remembers the start and end states of the cube.
  */
case class Transform(before:Cube, operations:Seq[Op] = Seq.empty) {

  /**
    * Immediately work out what the cube would look like after applying these operations
    */
  val after = before.applySeq(operations)

  /**
    * Creates a short string, suitable for printing
    *
    * @return
    */
  def short = {
    val sb = new mutable.StringBuilder
    for { op <- operations } sb.append(op.short)
    sb.toString()
  }

  /**
    * Is the cube completely solved?
    */
  def isSolved:Boolean = after == Cube.fresh

  /**
    * When we pass in lists of moves, we can pass in "Algorithms" (pre-discovered move sequences). This extracts an
    * Alg from a Transform
    */
  def alg = Alg(operations)

  /**
    * How many individual turns are there in this algorithm?
    */
  def countTurns:Int = operations.map(_.countTurns).sum

}

/**
  * The Solver, that you'll need to finish implementing.
  *
  * First, here are some hints.
  * 1. We NEVER re-orient the cube. There's no method for it.
  *    The top centre square is always white, the front centre square is always green.
  *
  * 2. There's lots of occasions where you'll search for an algorithm. To do that, start with a fresh cube and work out
  *    what the algorithm ought to turn the fresh cube into. Normally you won't want to check every square, but
  *    say things like "the bottom corners have been cycled, but the top and middle layers are unchanged"
  *
  * 3. In the end, you'll have a bunch of stages, each of which produces an Option[Transform].
  *    These get strung together using for notation, so you might like to review what for { a <- Some(1) } does
  *
  *
  *
  */
object Solver {

  /**
    * This little bit of notation is a type alias. It says "A cube test is a function from a Cube to a Boolean".
    * You'll see I've written a few functions that take CubeTests, and a helper for applying multiple CubeTests.
    *
    * So, where you see "CubeTest", remember "function from Cube to Boolean".
    */
  type CubeTest = Cube => Boolean

  /**
   * Heres an example CubeTest
   */
  val tnEdgeCorrect = {
    c:Cube => c.edgePiece(Top, North) == Cube.fresh.edgePiece(Top, North)
  }

  /**
    * And another. This checks the Top North edge piece (Top, North, middle) is in the correct position, though it
    * might not be the right way around.
    */
  val tnEdgeInPlace = {
    c:Cube => c.edgePiece(Top, North).isOrientationOf(Cube.fresh.edgePiece(Top, North))
  }


  /**
    * Your first task: write a helper function that will take a Seq of CubeTests, and produce a CubeTest that checks they are all true
    */
  def all(cond:Seq[CubeTest]):CubeTest = {
    c:Cube => cond.forall(_(c))
  }

  /**
    * Your next task: write a helper function that will take a Seq of CubeTests, and produce a CubeTest that checks if any of them are true
    */
  def any(cond:Seq[CubeTest]):CubeTest = {
    c:Cube => cond.exists(_(c))
  }

  /**
    * Your second task -- implement a tail recursive search for a Transform that matches the condition.
    * NB: Uncomment the @tailrec annotation when you implement it. This one should be tail recursive
    *
    * Note: For efficiency's sake, we're going to generate the moves just by counting... ie, you call it for
    * iteration 0, then recursively for iteration 1, then 2... That number will get turned into a unique sequence of moves.
    *
    * Cube.apply(num, fromMoves) takes a number and a list of allowed moves. It then turns the number into a unique
    * sequence of moves (from the list) and applies them. Internally it does this using a mutable copy of the cube, so it's fast.
    * It can process about 250,000 turns per second on my laptop.
    *
    * Transform.fromNum(num, fromMoves) will turn num into a Transform, containing the sequence of moves that Cube.apply(num, fromMoves) performs.
    *
    */
  @tailrec
  def search(cube:Cube, max:Int, cond:CubeTest, iteration:Int = 0, fromMoves:Seq[Op]):Option[Transform] = {
    val candidate = Transform(cube, Turn.fromNum(iteration, fromMoves))
    if (iteration > max) return None
    else if (cond(candidate.after)) return Some(candidate)
    else search(cube, max, cond, iteration + 1, fromMoves)
  }

  /**
    * Rather than call your recursive function directly, I suggest you go through this one. It'll call your recursive
    * function but also print out what it finds, and print the cube's finish position at the end of any successful
    * sequence of moves it finds.
    */
  def searchAndPrint(name:String, cube:Cube, cond:CubeTest, fromMoves:Seq[Op] = Turn.allTurns, max:Int = Math.pow(12, 6).toInt):Option[Transform] = {
    // First we catch the case where it's already true!
    if (cond(cube)) {
      println(name + ": already true")
      println(cube)
      Some(Transform(cube))
    } else {
      search(cube, max, cond, 0, fromMoves) match {
        case Some(t) => {
          println(name + ": " + t.short)
          println(t.after)
          Some(t)
        }
        case None => {
          println(name + ": not found")
          None
        }
      }
    }
  }

  /**
    * Often, we'll want to test if an edge piece is "in place" -- in the right position, but not necessarily the right
    * orientation.
    *
    * Here, you need to write a function that will return a CubeTest (ie, will return a function from Cube to Boolean)
    * Hint: compare it to the same position in Cube.fresh
    */
  def edgePieceInPlace(s:Side, e:Edge):CubeTest = {
    c:Cube => Cube.fresh.edgePiece(s, e).isOrientationOf(c.edgePiece(s, e))
  }

  /**
    * Often, we'll want to test if an edge piece is "correct" -- in the right position and the right
    * orientation.
    *
    * Here, you need to write a function that will return a CubeTest (ie, will return a function from Cube to Boolean)
    * Hint: compare it to the same position in Cube.fresh
    */
  def edgePieceCorrect(s:Side, e:Edge):CubeTest = {
    c:Cube => Cube.fresh.edgePiece(s, e) == c.edgePiece(s, e)
  }

  /**
    * Often, we'll want to test if a corner piece is "in place" -- in the right position, but not necessarily the right
    * orientation.
    *
    * Here, you need to write a function that will return a CubeTest (ie, will return a function from Cube to Boolean)
    * Hint: compare it to the same position in Cube.fresh
    */
  def cornerPieceInPlace(tb:TB, lr:LR, fb:FB):CubeTest = {
    c:Cube => Cube.fresh.cornerPiece(tb, lr, fb).isOrientationOf(c.cornerPiece(tb, lr, fb))
  }

  /**
    * Often, we'll want to test if a corner piece is "correct" -- in the right position and the right
    * orientation.
    *
    * Here, you need to write a function that will return a CubeTest (ie, will return a function from Cube to Boolean)
    * Hint: compare it to the same position in Cube.fresh
    */
  def cornerPieceCorrect(tb:TB, lr:LR, fb:FB):CubeTest = {
    c:Cube => Cube.fresh.cornerPiece(tb, lr, fb) == c.cornerPiece(tb, lr, fb)
  }

  /**
    * Step 1 is to "solve the white edges", by making a white cross on the top of the cube.
    * Write a CubeTest using your previous functions to see if you've done that
    */
  val whiteCrossCorrect: CubeTest = {
    all(Seq(
      edgePieceCorrect(Top, North),
      edgePieceCorrect(Top, East),
      edgePieceCorrect(Top, South),
      edgePieceCorrect(Top, West)
    ))
  }

  /**
    * Now to solve the cross, we'll progressively call your search function, first putting an edge in place, then
    * orienting it correctly. Note how each time, we check the previous conditions too in the search, so we make sure
    * the new transform isn't mucking up what we've solved so far.
    *
    * I've already written this one for you
    */
  def solveCross(cube:Cube) = for {
    a <- Solver.searchAndPrint("Position TN edge", cube, edgePieceInPlace(Top, North))
    b <- Solver.searchAndPrint("Orient TN edge", a.after, edgePieceCorrect(Top, North))
    c <- Solver.searchAndPrint("Position TS edge", b.after, all(Seq(edgePieceCorrect(Top, North), edgePieceInPlace(Top, South))))
    d <- Solver.searchAndPrint("Orient TS edge", c.after, all(Seq(edgePieceCorrect(Top, North), edgePieceCorrect(Top, South))))
    e <- Solver.searchAndPrint("Position TE edge", d.after, all(Seq(edgePieceInPlace(Top, North), edgePieceCorrect(Top, South), edgePieceInPlace(Top, East))))
    f <- Solver.searchAndPrint("Orient TE edge", e.after, all(Seq(edgePieceInPlace(Top, North), edgePieceCorrect(Top, South), edgePieceCorrect(Top, East))))
    g <- Solver.searchAndPrint("Position TW edge", f.after, all(Seq(edgePieceInPlace(Top, North), edgePieceCorrect(Top, South), edgePieceCorrect(Top, East), edgePieceInPlace(Top, West))))
    h <- Solver.searchAndPrint("Orient TW edge", g.after, whiteCrossCorrect)
  } yield {
    Transform(cube, Seq(a, b, c, d, e, f, g, h).map(_.alg))
  }



  /**
    * Stage 2: Solve the white corners
    *
    * First, we need to position a white corner *underneath* where it should go -- ie, position it on the bottom in the same place
    *
    * Write us a function that will return a CubeTest for whether the correct corner piece is underneath the corner it should go into.
    * (It needn't be oriented correctly.)
    */
  def whiteCornerUnder(lr:LR, fb:FB):CubeTest = {
    c:Cube => Cube.fresh.cornerPiece(Top, lr, fb).isOrientationOf(c.cornerPiece(Bottom, lr, fb))
  }

  /**
    * The R' D' R D algorithm
    */
  val fbrCornerToTop1 = Alg(Seq(Turn.Ra, Turn.Da, Turn.R, Turn.D))

  /** Test for whether the top corners are in the correct locations and orientations */
  val topCornersCorrect = all(Seq(cornerPieceCorrect(Top, Right, Front), cornerPieceCorrect(Top, Right, Back), cornerPieceCorrect(Top, Left, Back), cornerPieceCorrect(Top, Left, Front)))

  /** Hint: if the top edges and corners are correct, the top is correct */
  val topCorrect:CubeTest = all(Seq(topCornersCorrect, whiteCrossCorrect))

  /**
    * This function takes a cube where Stage 1 has already been solved. And uses your search and tests to do stage 2.
    *
    * Note in this that as we move around the cube to look at the other corners, I'm calling
    * fbr.CornerToTop1.rotateClockwise -- "rotating the algorithm" rather than rotating the cube.
    *
    * But I've already written this part for you
    */
  def solveTop(cube:Cube) = for {
    fr <- Solver.searchAndPrint("FR corner under", cube, all(Seq(whiteCrossCorrect, whiteCornerUnder(Right, Front))))
    tfr <- Solver.searchAndPrint("TFR corner correct", fr.after, all(Seq(whiteCrossCorrect, cornerPieceCorrect(Top, Right, Front))), Seq(fbrCornerToTop1))
    br <- Solver.searchAndPrint("BR corner under", tfr.after, all(Seq(whiteCrossCorrect, cornerPieceCorrect(Top, Right, Front), whiteCornerUnder(Right, Back))))
    tbr <- Solver.searchAndPrint("TBR corner correct", br.after, all(Seq(whiteCrossCorrect, cornerPieceCorrect(Top, Right, Front), cornerPieceCorrect(Top, Right, Back))), Seq(fbrCornerToTop1.rotateClockwise))
    bl <- Solver.searchAndPrint("BL corner under", tbr.after, all(Seq(whiteCrossCorrect, cornerPieceCorrect(Top, Right, Front), cornerPieceCorrect(Top, Right, Back), whiteCornerUnder(Left, Back))))
    tbl <- Solver.searchAndPrint("TBL corner correct", bl.after, all(Seq(whiteCrossCorrect, cornerPieceCorrect(Top, Right, Front), cornerPieceCorrect(Top, Right, Back), cornerPieceCorrect(Top, Left, Back))), Seq(fbrCornerToTop1.rotateClockwise.rotateClockwise))
    fl <- Solver.searchAndPrint("FL corner under", tbl.after, all(Seq(whiteCrossCorrect, cornerPieceCorrect(Top, Right, Front), cornerPieceCorrect(Top, Right, Back), cornerPieceCorrect(Top, Left, Back), whiteCornerUnder(Left, Front))))
    tfl <- Solver.searchAndPrint("TFL corner correct", fl.after, topCorrect, Seq(fbrCornerToTop1.rotateClockwise.rotateClockwise.rotateClockwise))
  } yield {
      Transform(cube, Seq(fr, tfr, br, tbr, bl, tbl, fl, tfl).map(_.alg))
  }


  /*
   * Stage 3: The Middle layer
   *
   * The instructions say we should turn the cube upside down. But we can't.
   * So we're going to search for some move sequences that can do the edge swaps with the cube the right way up
   */

  /**
    * We're going to find our own edge-swapping algorithms. Start with a fresh cube, and search for a transform that
    * has the edges swapped but the top layer still intact.
    *
    * I've done this one; later, you'll need to do this to find some other algorithms...
    */
  def findEdgeSwap(s1:Side, e1:Edge, s2:Side, e2:Edge) = {
    Solver.searchAndPrint(
      s"Finding edge swap for $s1,$e1 -> $s2,$e2",
      Cube.fresh, all(Seq(topCorrect, _.edgePiece(s1, e1) == Cube.fresh.edgePiece(s2, e2)))
    )
  }

  /*
   * Now to solve the middle layer, we're just going to do some searching. But instead of using every possible move,
   * we're just going to use moves we know don't affect the top layer:
   * Rotating the bottom, and our edge swap algorithms.
   */

  val feCorrect = edgePieceCorrect(Front, East)
  val feReCorrect = all(Seq(feCorrect, edgePieceCorrect(Right, East)))
  val feReBeCorrect = all(Seq(feCorrect, feReCorrect, edgePieceCorrect(Back, East)))
  val feReBeLeCorrect = all(Seq(feCorrect, feReCorrect, feReBeCorrect, edgePieceCorrect(Left, East)))

  /**
    * This checks that the top and middle layers are both solved
    */
  val topMiddleCorrect: CubeTest = all(Seq(topCorrect, feReBeLeCorrect))

  /**
    * This function again uses your search and conditions to solve the middle layer
    */
  def solveEdges(c:Cube) = for {
    feSwapT <- findEdgeSwap(Front, East, Front, South)
    fwSwapT <- findEdgeSwap(Front, West, Front, South)

    feSwap = feSwapT.alg
    fwSwap = fwSwapT.alg
    reSwap = feSwap.rotateClockwise
    rwSwap = fwSwap.rotateClockwise
    beSwap = reSwap.rotateClockwise
    bwSwap = rwSwap.rotateClockwise
    leSwap = beSwap.rotateClockwise
    lwSwap = bwSwap.rotateClockwise

    // Here's our top-preserving operations
    ops = Seq(feSwap, fwSwap, reSwap, rwSwap, beSwap, bwSwap, leSwap, lwSwap, Turn.D, Turn.Da)

    fec <- searchAndPrint("FE edge", c, feCorrect, ops)
    feRec <- searchAndPrint("RE edge", fec.after, feReCorrect, ops)
    feReBec <- searchAndPrint("BE edge", feRec.after, feReBeCorrect, ops)
    feReBeLec <- searchAndPrint("LE edge", feReBec.after, feReBeLeCorrect, ops)
  } yield {
    Transform(c, Seq(fec, feRec, feReBec, feReBeLec).map(_.alg))
  }


  /*
   * Stage 4: The Yellow Cross
   *
   * Next we need to work on the bottom layer. Don't forget, we still have the white side at the top!
   */

  /**
    *  First, we need a condition to see if we have a yellow cross on the bottom
    */
  val yellowCross:CubeTest = {
    c: Cube => {
      val b = c.bottom
      b.nm == Y &&
        b.me == Y &&
        b.sm == Y &&
        b.mw == Y
    }
  }

  /**
    * Again, we can't just pluck the algorithm from the page because it's upside down.
    * So we're going to search for an algorithm that will change the number of yellow edge squares on the bottom of a fresh
    * cube without altering the other layers.
    *
    * Looking at the sheet, the algorithm we're looking for would turn a line into a cross. Since we're starting with
    * the complete yellow cross, let's look for one that produces a line.
    *
    * I've done this one for you
    */
  val yellowLineNotCross:CubeTest = { cube =>
    val bot = cube.bottom
    bot.nm != Y && bot.me == Y && bot.sm != Y && bot.mw == Y
  }

  /**
    * Now, we need to find an algorithm that will manipulate the yellow cross squares how we want, without affecting the
    * top or middle layers...
    *
    * Your turn to implement this one...
    */
  def findCrossMove = Solver.searchAndPrint("Cross move", Cube.fresh, all(Seq(topMiddleCorrect, yellowLineNotCross)))

  /**
    * This function uses the algorithm you just looked for
    *
    * It takes a cube where the top and middle layers have been solved, and finds a transform to produce one with the
    * yellow cross on the bottom.
    *
    * You'll notice I set fromMoves=Seq(cm.alg, Turn.D, Turn.Da) -- that is, we are only using the moves we want that
    * won't affect the other two layers (our cross algorithm, and rotating the bottom)
    */
  def solveBottomCross(c:Cube) = for {
    cm <- findCrossMove
    cross <- searchAndPrint("Yellow cross", c, all(Seq(topMiddleCorrect, yellowCross)), fromMoves=Seq(cm.alg, Turn.D, Turn.Da))
  } yield cross


  /*
   * Stage 5: Swap Yellow Layer Edges
   */

  /**
    * Now we need to find out how to swap two bottom edges without affecting the rest.
    * Particularly, we want bottom.sm -> bottom.mw, but we want the north and west edge pieces unchanged.
    *
    * We're on the "tricky bottom layer", but we're really just searching and testing like before.
    */
  val bottomSmToMw:CubeTest = {
    c =>
      c.edgePiece(Bottom, West).isOrientationOf(Cube.fresh.edgePiece(Bottom, South)) &&
      edgePieceCorrect(Bottom, East)(c) && edgePieceCorrect(Bottom, North)(c)
  }

  /**
    * And now we're going to search for our bottom edge move, because the one on the page is for if the cube was the
    * other way up.
    *
    * If we allowed all possible moves, your search would take too long. So limit the fromMoves set to R, R', D, and D'
    * (Turn.R, Turn.Ra, Turn.D, and Turn.Da)
    */
  def findBottomSmToMwMove =  Solver.searchAndPrint("Bottom edge swap", Cube.fresh, bottomSmToMw, Seq(Turn.R, Turn.Ra, Turn.D, Turn.Da))

  /**
    * Now we need a CubeTest to see if the bottom edges are correct. You can do this one.
    */
  val bottomEdgesCorrect:CubeTest = {
    all(Seq(
      edgePieceCorrect(Bottom, North),
      edgePieceCorrect(Bottom, East),
      edgePieceCorrect(Bottom, South),
      edgePieceCorrect(Bottom, West)
    ))
  }

  /**
    * This function should take a cube with the top and middle layers solved, and search for a transform that will
    * set the bottom edges correctly.
    *
    * Note: Limit the fromMoves to your bottomSmToMwMove algorithm, Turn.D, and Turn.Da
    */
  def solveBottomEdgePieces(c:Cube) = for {
    cm <- findBottomSmToMwMove
    e <- searchAndPrint("Bottom edge pieces", c, bottomEdgesCorrect, fromMoves = Seq(cm.alg, Turn.D, Turn.Da))
  } yield e

  /*
   * Stage 6: Position Yellow Layer Corners
   */

  /**
    * I've done this one for you because it's easy to make a silly mistake
    *
    * Check that bottom.front.right has moved to bottom back left,
    * bottom back left to bottom back right
    * bottom back right to bottom front left
    *
    * And don't forget it's just isOrentationOf -- they might be turned around
    */
  val bottomCornersCycled:CubeTest = { c =>
    c.cornerPiece(Bottom, Left, Back).isOrientationOf(Cube.fresh.cornerPiece(Bottom, Right, Front)) &&
      c.cornerPiece(Bottom, Right, Back).isOrientationOf(Cube.fresh.cornerPiece(Bottom, Left, Back)) &&
      c.cornerPiece(Bottom, Right, Front).isOrientationOf(Cube.fresh.cornerPiece(Bottom, Right, Back))
  }

  /**
    * Now we need to search for a move that will cycle the bottom corners, but leave everything else unchanged
    *
    * Again, to shorten the search, note it only uses D, D', R, R', L, L'
    */
  def findCycleBottomCornersMove:Option[Transform] =  Solver.searchAndPrint("Bottom edge swap", Cube.fresh, all(Seq(topMiddleCorrect, bottomEdgesCorrect, bottomCornersCycled)), Seq(Turn.L, Turn.La, Turn.R, Turn.Ra, Turn.D, Turn.Da))


  /**
    * You can write the CubeTest that would see if all the corners are in the correct place
    */
  val bottomCornersInPlace:CubeTest = all(Seq(cornerPieceInPlace(Bottom, Left, Front), cornerPieceInPlace(Bottom, Right, Front), cornerPieceInPlace(Bottom, Left, Back), cornerPieceInPlace(Bottom, Right, Back)))

  /**
    * This function should take a cube that's reached Stage 5, and search for a transform that will place the bottom
    * corners (without mucking up the top or middle layers, or the bottom edges)
    *
    * Again, I suggest you limit the fromMoves that get tried...
    */
  def placeBottomCorners(c:Cube) = for {
    cm <- findCycleBottomCornersMove
    e <- searchAndPrint("Position bottom corners", c, all(Seq(topMiddleCorrect,bottomCornersInPlace,bottomEdgesCorrect)), Seq(Turn.L, Turn.La, Turn.R, Turn.Ra, Turn.D, Turn.Da))
  } yield e

  /*
   * Stage 7: Orienting the Yellow corners
   */

  /**
    * Ok, that's enough searching for algorithms. The instructions say to use the corner swapping algorithm from earlier.
    * Our cube is upside down, so let's turn the algorithm upside down. I've done this one.
    */
  val rotateBottomCorner = fbrCornerToTop1.rotateDown.rotateDown

  /**
    * And now, I'd better leave the last part to you...
    *
    * Taking a cube that's been solved to Stage 6, search for a transform that will finish it. For this one, we should
    * only need rotateBottomCorner, Turn.D and Turn.Da
    */
  def solveLastCorners(c:Cube) = for {
    fr <- searchAndPrint("Bottom Front Right corner", c,
      all(Seq(topMiddleCorrect, bottomEdgesCorrect, bottomCornersInPlace, cornerPieceCorrect(Bottom, Right, Front))),
      Seq(rotateBottomCorner, Turn.D, Turn.Da)
    )
    br <- searchAndPrint("Bottom Right Back corner", fr.after,
      all(Seq(topMiddleCorrect, bottomEdgesCorrect, bottomCornersInPlace, cornerPieceCorrect(Bottom, Right, Front), cornerPieceCorrect(Bottom, Right, Back))),
      Seq(rotateBottomCorner, Turn.D, Turn.Da)
    )
    bl <- searchAndPrint("Bottom Left Back corner", br.after,
      all(Seq(topMiddleCorrect, bottomEdgesCorrect, bottomCornersInPlace, cornerPieceCorrect(Bottom, Right, Front), cornerPieceCorrect(Bottom, Right, Back), cornerPieceCorrect(Bottom, Left, Back))),
      Seq(rotateBottomCorner, Turn.D, Turn.Da)
    )
    done <- searchAndPrint("THE LAST CORNER!", bl.after,
      all(Seq(topMiddleCorrect, bottomEdgesCorrect, bottomCornersInPlace, cornerPieceCorrect(Bottom, Right, Front), cornerPieceCorrect(Bottom, Right, Back), cornerPieceCorrect(Bottom, Left, Back), cornerPieceCorrect(Bottom, Left, Front))),
      Seq(rotateBottomCorner, Turn.D, Turn.Da)
    )
  } yield {
    Transform(c, Seq(fr, br, bl, done).map(_.alg))
  }

  /**
    * And if you've managed those, here now is your complete Rubik's Cube Solving function!
    */
  def solveMyCube(cube:Cube) = for {
    a <- solveCross(cube)
    b <- solveTop(a.after)
    c <- solveEdges(b.after)
    d <- solveBottomCross(c.after)
    e <- solveBottomEdgePieces(d.after)
    f <- placeBottomCorners(e.after)
    g <- solveLastCorners(f.after)
  } {
    def composed = Alg(Seq(a, b, c, d, e, f, g).map(_.alg))

    println(s"Solved in ${composed.countTurns} moves: ${composed.short}")
  }

}
