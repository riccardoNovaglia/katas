package elevator

protected class Elevator(val currentFloor: Int, val destinations: List[Int], val state: ElevatorState) extends TimedEntity {
  protected def callAt(floor: Int): Elevator = {
    if (destinations.nonEmpty && Range(currentFloor, destinations.head).inclusive.contains(floor)) {
      return Elevator(currentFloor, floor :: destinations)
    }
    val newDestinations = destinations :+ floor
    Elevator(currentFloor, if (newDestinations.max < currentFloor) newDestinations.sorted.reverse else newDestinations.sorted)
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
      if (destinations.isEmpty) this else Elevator(currentFloor, destinations, OPEN)
    }
  }

  private lazy val move = if (destinations.head > currentFloor) currentFloor + 1 else currentFloor - 1

  override def toString: String = s"Currently: floor [$currentFloor], doors [$state] - Destinations: [$destinations]"
}

object Elevator {
  def apply(currentFloor: Int = 0, destinations: List[Int] = List(), state: ElevatorState = CLOSED): Elevator = {
    val elevator = new Elevator(currentFloor, destinations, state)
    TickerClock.add(elevator)
    elevator
  }

  def callAt(floor: Int): Elevator = {
    val newElevator = TickerClock.getCurrent.asInstanceOf[Elevator].callAt(floor)
    TickerClock.add(newElevator)
    newElevator
  }
}

trait ElevatorState {
  override def toString: String = this.getClass.getName
}

object CLOSED extends ElevatorState

object OPEN extends ElevatorState
