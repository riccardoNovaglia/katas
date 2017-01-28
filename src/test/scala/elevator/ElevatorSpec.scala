package elevator

import org.scalatest.{FlatSpec, Matchers}

class ElevatorSpec extends FlatSpec with Matchers {

  //  These are some features. They can be implemented in any order you prefer.
  //    * an elevator responds to calls containing a source floor and direction
  //    * an elevator delivers passengers to requested floors
  //    * you may implement current floor monitor
  //    * you may implement direction arrows
  //    * you may implement doors (opening and closing)
  //    * there can be more than one elevator
  "An Elevator" should "start at ground floor" in {
    Elevator().currentFloor should be(0)
  }

  it should "only move when the clock ticks" in {
    Elevator().goTo(1).currentFloor should be(0)
    shouldNowBeAtFloor(1)
  }

  it should "move one floor at a time" in {
    Elevator().goTo(2)
    shouldNowBeAtFloor(1)
    shouldNowBeAtFloor(2)
  }

  it should "be able to go down floors" in {
    Elevator().goTo(-1)
    shouldNowBeAtFloor(-1)
  }

  it should "serve one request at a time" in {
    Elevator().goTo(2).goTo(0)
    shouldNowBeAtFloor(1)
    shouldNowBeAtFloor(2)
    shouldNowBeAtFloor(1)
    shouldNowBeAtFloor(0)
  }

  private def shouldNowBeAtFloor(expectedFloor: Int) = {
    val newElevator: Elevator = TickerClock.tick().asInstanceOf[Elevator]
    withClue(s"Expected elevator to be at floor $expectedFloor but found at ${newElevator.currentFloor}") {
      newElevator.currentFloor should be(expectedFloor)
    }
  }
}
