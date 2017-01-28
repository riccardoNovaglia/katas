package elevator

import org.scalatest.{FlatSpec, Matchers}

class ElevatorSpec extends FlatSpec with Matchers {

  "An Elevator" should "start at ground floor" in {
    Elevator().currentFloor should be(0)
  }

  it should "only move when the clock ticks" in {
    Elevator.goTo(1).currentFloor should be(0)
    shouldNowBeAtFloor(1)
  }

  it should "move one floor at a time" in {
    Elevator.goTo(2)
    shouldNowBeAtFloor(1)
    shouldNowBeAtFloor(2)
  }

  it should "be able to go down floors" in {
    Elevator.goTo(-1)
    shouldNowBeAtFloor(-1)
  }

  it should "start closed" in {
    Elevator().state should be(CLOSED)
  }

  it should "open the doors when it's called at its current floor and the clock ticks" in {
    Elevator.callAt(0).state should be(CLOSED)
    shouldNowBe(OPEN)
  }

  it should "serve one request at a time and let people off when it reaches its destination" in {
    Elevator.goTo(0, Elevator.goTo(2))
    shouldNowBeAtFloor(1)
    shouldNowBeAtFloor(2)
    shouldNowBe(OPEN)
    shouldNowBe(CLOSED)
    shouldNowBeAtFloor(1)
    shouldNowBeAtFloor(0)
  }

  it should "stop when called at a floor on its way to a destination" in {
    Elevator.goTo(0, Elevator.goTo(3))
    val elevator = shouldNowBeAtFloor(1)
    Elevator.callAt(2, elevator)
    shouldNowBeAtFloor(2)
    shouldNowBe(OPEN)
    shouldNowBe(CLOSED)
    shouldNowBeAtFloor(3)
    shouldNowBe(OPEN)
    shouldNowBe(CLOSED)
  }

  private def shouldNowBeAtFloor(expectedFloor: Int) = {
    val newElevator: Elevator = TickerClock.tick().asInstanceOf[Elevator]
    withClue(s"Expected elevator to be at floor $expectedFloor but found at ${newElevator.currentFloor}") {
      newElevator.currentFloor should be(expectedFloor)
    }
    newElevator
  }

  private def shouldNowBe(expectedState: ElevatorState) = {
    val newElevator: Elevator = TickerClock.tick().asInstanceOf[Elevator]
    withClue(s"Expected elevator to be $expectedState but is actually ${newElevator.state}") {
      newElevator.state should be(expectedState)
    }
  }
}
