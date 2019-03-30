package physics

import java.awt.geom.Line2D

object Physics {
  def computePotentialLocation(thing: PhysicalObject, deltaTime: Double): PhysicsVector ={
    // Location
    val xPos: Double = thing.location.x
    val yPos: Double = thing.location.y
    val zPos: Double = thing.location.z
    // Velocity
    val xVel: Double = thing.velocity.x
    val yVel: Double = thing.velocity.y
    val zVel: Double = thing.velocity.z
    // PotentialLocation Calculator
    val newXPos: Double = xPos + xVel * deltaTime
    val newYPos: Double = yPos + yVel * deltaTime
    var newZPos: Double = zPos + zVel * deltaTime
    if (newZPos < 0.0){
      newZPos = 0.0
    }
    // PotentialLocation
    val possibleLocation = new PhysicsVector(newXPos, newYPos, newZPos)
    possibleLocation
  }

  def updateVelocity(thing: PhysicalObject, earth: World, deltaTime: Double): Unit ={
    // Take only z acceleration
    var zVel: Double = thing.velocity.z
    // Update Velocity Calculator
    zVel = zVel - earth.gravity * deltaTime
    // If object is on the ground with negative acceleration
    if (thing.location.z == 0.0 && thing.velocity.z < 0.0){
      zVel = 0.0
    }
    if (thing.location.z == 0.0 && thing.velocity.z < earth.gravity){
      zVel = 0.0
    }
    // Update Velocity
    thing.velocity.z = zVel
  }

  def detectCollision(thing: PhysicalObject, potLocation: PhysicsVector, edgy: Boundary): Boolean ={
    // Boundary Points
    val x1: Double = edgy.start.x; val y1: Double = edgy.start.y
    val x2: Double = edgy.end.x; val y2: Double = edgy.end.y
    // Location Lines
    val x3: Double = thing.location.x; val y3: Double = thing.location.y
    val x4: Double = potLocation.x; val y4: Double = potLocation.y
    // Create Boundary Lines
    val tallWall: Line2D = new Line2D.Double(x1, y1, x2, y2)
    val objectLine: Line2D = new Line2D.Double(x3, y3, x4, y4)
    // Check if lines intersect
    val result1: Boolean = objectLine.intersectsLine(tallWall)
    // Final Crossing Check
    var doNotCross: Boolean = true
    if (result1){
      doNotCross = false
    }
    doNotCross
  }

  def updateWorld(earth: World, deltaTime: Double): Unit ={
    for (everyThing <- earth.objects){
      val originalX = everyThing.location.x
      val originalY = everyThing.location.y
      updateVelocity(everyThing, earth, deltaTime)
      val potLocation: PhysicsVector = computePotentialLocation(everyThing, deltaTime)
      for (wall <- earth.boundaries){
        if (!detectCollision(everyThing, potLocation, wall)){
          everyThing.location.x = originalX
          everyThing.location.y = originalY
          everyThing.location.z = potLocation.z
        }
        else if (detectCollision(everyThing, potLocation, wall)){
          everyThing.location.x = potLocation.x
          everyThing.location.y = potLocation.y
          everyThing.location.z = potLocation.z
        }
      }
    }
  }
}
