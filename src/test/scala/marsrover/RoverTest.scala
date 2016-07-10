package marsrover

import org.scalatest.{FlatSpec, Matchers}

/*
You are given the initial starting point (x,y) of a rover and the direction (N,S,E,W) it is facing.
The rover receives a character array of commands.
Implement commands that move the rover forward/backward (f,b).
Implement commands that turn the rover left/right (l,r).
Implement wrapping from one edge of the grid to another. (planets are spheres after all)
Implement obstacle detection before each move to a new square.
If a given sequence of commands encounters an obstacle, the rover moves up to the last possible point and reports the obstacle.
 */


class RoverTest extends FlatSpec with Matchers {

  def withNewRover(testCode: Rover => Any) = {
    testCode(new DefaultRover())
  }

  "The Mars Rover" should "start at 0,0 facing North" in withNewRover { rover =>
    rover.coordinates shouldBe(0, 0)
    rover.direction shouldBe North
    rover.location shouldBe Location((0, 0), North)
  }

  it should "be able to start at different positions facing a different direction" in {
    val rover = new Rover((1, 2), West)
    rover.coordinates shouldBe(1, 2)
    rover.direction shouldBe West
    rover.location shouldBe Location((1, 2), West)
  }

  it should "move up when facing N and moving forward" in withNewRover { rover =>
    rover.execute(List('f')).coordinates shouldBe(0, 1)
  }

  it should "be able to change direction" in withNewRover { rover =>
    rover.execute(List('r')).direction shouldBe East
  }

  it should "be able to turn multiple times" in withNewRover { rover =>
    rover.execute(List('r', 'r')).direction shouldBe South
  }

  it should "be able to move forward multiple times" in withNewRover { rover =>
    rover.execute(List('f', 'f')).coordinates shouldBe(0, 2)
  }

  it should "be able to go around for a while" in withNewRover { rover =>
    rover.execute(List('f', 'r', 'f', 'l', 'f', 'l')).location shouldBe Location((1, 2), West)
  }

  it should "be able to go backwards" in withNewRover { rover =>
    rover.execute(List('b')).coordinates shouldBe(0, -1)
  }
}
