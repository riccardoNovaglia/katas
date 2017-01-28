package elevator

import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class ElevatorSpec extends FlatSpec with Matchers with OneInstancePerTest {

  //  These are some features. They can be implemented in any order you prefer.
  //    * an elevator responds to calls containing a source floor and direction
  //    * an elevator delivers passengers to requested floors
  //    * you may implement current floor monitor
  //    * you may implement direction arrows
  //    * you may implement doors (opening and closing)
  //    * there can be more than one elevator
  val elevator = Elevator()

  "An Elevator" should "start at ground floor" in {
    elevator.currentFloor should be(0)
  }

  it should "only move when the clock ticks" in {
    elevator.currentFloor should be(0)
    elevator.goTo(1).currentFloor should be(0)
    shouldNowBeAtFloor(1)
  }

  it should "move one floor at a time" in {
    elevator.goTo(2)
    shouldNowBeAtFloor(1)
    shouldNowBeAtFloor(2)
  }

  it should "be able to go down floors" in {
    elevator.goTo(-1)
    shouldNowBeAtFloor(-1)
  }

  it should "serve one request at a time" in {
    elevator.goTo(2).goTo(0)
    shouldNowBeAtFloor(1)
    shouldNowBeAtFloor(2)
    shouldNowBeAtFloor(1)
    shouldNowBeAtFloor(0)
  }

  private def shouldNowBeAtFloor(expectedFloor: Int) = {
    TickerClock.tick()
    withClue(s"Expected elevator to be at floor $expectedFloor but found at ${elevator.currentFloor}") {
      elevator.currentFloor should be(expectedFloor)
    }
  }
}
