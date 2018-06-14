package com.nagytech.rubik

/**
  * Each face has four edges. Let's call them North, East, South, and West
  */
sealed abstract class Edge
object North extends Edge
object East extends Edge
object South extends Edge
object West extends Edge
object Edge {
  def edges = Seq(North, East, South, West)

  def except(edge:Edge) = edges.filter(_ != edge)
}

