package elevator

import org.scalatest.{FlatSpec, Matchers}

class ElevatorSpec extends FlatSpec with Matchers {

  "An Elevator" should "start at ground floor" in {
    Elevator().currentFloor should be(0)
  }

  it should "only move when the clock ticks" in {
    Elevator()
    Elevator.callAt(1).currentFloor should be(0)
    shouldNowBeAtFloor(1)
  }

  it should "move one floor at a time" in {
    Elevator()
    Elevator.callAt(2)
    shouldNowBeAtFloor(1)
    shouldNowBeAtFloor(2)
  }

  it should "be able to go down floors" in {
    Elevator()
    Elevator.callAt(-1)
    shouldNowBeAtFloor(-1)
  }

  it should "start closed" in {
    Elevator().state should be(CLOSED)
  }

  it should "open the doors when it's called at its current floor and the clock ticks" in {
    Elevator.callAt(0).state should be(CLOSED)
    shouldOpenAndClose()
  }

  it should "stop when called at a floor on its way to a destination" in {
    Elevator()
    Elevator.callAt(3)
    shouldNowBeAtFloor(1)
    Elevator.callAt(2)
    shouldNowBeAtFloor(2)
    shouldOpenAndClose()
    shouldNowBeAtFloor(3)
    shouldOpenAndClose()
  }

  it should "remain closed once it reaches the correct floor" in {
    Elevator().state should be(CLOSED)
    shouldNowBe(CLOSED)
    shouldNowBe(CLOSED)
  }

  it should "stop at destinations in order" in {
    Elevator()
    Elevator.callAt(2)
    Elevator.callAt(1)
    shouldNowBeAtFloor(1)
    shouldOpenAndClose()
    shouldNowBeAtFloor(2)
    shouldOpenAndClose()
  }

  it should "stop at destinations in reverse order if it's already above them" in {
    Elevator()
    Elevator.callAt(3)
    shouldNowBeAtFloor(1)
    shouldNowBeAtFloor(2)
    shouldNowBeAtFloor(3)
    shouldOpenAndClose()
    Elevator.callAt(2)
    Elevator.callAt(1)
    shouldNowBeAtFloor(2)
    shouldOpenAndClose()
    shouldNowBeAtFloor(1)
    shouldOpenAndClose()
  }

  private def shouldOpenAndClose() = {
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
