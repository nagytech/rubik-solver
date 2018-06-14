package com.nagytech


import com.nagytech.rubik._

object App extends App {

  import Solver._

  var cube = Cube.fresh
  val chaos = cube.apply(43008370, Turn.allTurns)

  println("Here's a fresh cube:")
  println(cube)

  println("And here's our scrambled cube:")
  println(chaos)

  println("Now let's solve it:")
  solveMyCube(chaos)

}
