package elevator

protected class Elevator(val currentFloor: Int, val destinations: List[Int], val state: ElevatorState) extends TimedEntity {
  protected def goTo(floor: Int): Elevator = {
    Elevator(currentFloor, destinations :+ floor)
  }

  protected def callAt(floor: Int): Elevator = {
    if (destinations.nonEmpty) {
      if (Range(currentFloor, destinations.head).inclusive.contains(floor)) {
        return Elevator(currentFloor, floor :: destinations)
      }
    }
    Elevator(currentFloor, destinations :+ floor)
  }

  override def executeAction(): Elevator = {
    if (atTheRightFloor()) {
      verifyState
    } else {
      Elevator(move, destinations)
    }
  }

  private def atTheRightFloor(floor: Int = currentFloor) = destinations.isEmpty || destinations.head == floor

  private def verifyState = {
    if (state == OPEN) {
      Elevator(currentFloor, destinations.drop(1), CLOSED)
    } else {
      Elevator(currentFloor, destinations, OPEN)
    }
  }

  private lazy val move = if (destinations.head > currentFloor) currentFloor + 1 else currentFloor - 1
}

object Elevator {
  def apply(currentFloor: Int = 0, destinations: List[Int] = List(), state: ElevatorState = CLOSED): Elevator = {
    val elevator = new Elevator(currentFloor, destinations, state)
    TickerClock.add(elevator)
    elevator
  }

  def callAt(floor: Int, elevator: Elevator = Elevator.apply()): Elevator = {
    val newElevator = elevator.callAt(floor)
    TickerClock.add(newElevator)
    newElevator
  }

  def goTo(floor: Int, elevator: Elevator = Elevator.apply()): Elevator = {
    Elevator(elevator.currentFloor, elevator.destinations :+ floor, elevator.state)
  }
}

trait ElevatorState {
  override def toString: String = this.getClass.getName
}

object CLOSED extends ElevatorState

object OPEN extends ElevatorState
