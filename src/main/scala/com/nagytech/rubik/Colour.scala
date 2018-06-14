package com.nagytech.rubik

/**
  * The colours of the cube (white, red, green, blue, orange, yellow).
  * We just name the colours after their initial letter to keep things short.
  */
sealed trait Colour
case object W extends Colour
case object R extends Colour
case object G extends Colour
case object B extends Colour
case object O extends Colour
case object Y extends Colour
