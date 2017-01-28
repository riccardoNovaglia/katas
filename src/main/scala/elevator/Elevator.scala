package elevator

protected class Elevator(val currentFloor: Int, val destinations: List[Int]) extends TimedEntity {
  def goTo(floor: Int): Elevator = {
    Elevator(currentFloor, destinations :+ floor)
  }

  override def executeAction(): Elevator = {
    if (!atTheRightFloor()) {
      Elevator(move, updateDestinationsWith(move))
    } else {
      goToNextDestinationIfThereIsOne
    }
  }

  private def atTheRightFloor(floor: Int = currentFloor) = destinations.isEmpty || destinations.head == floor

  private lazy val move = if (destinations.head > currentFloor) currentFloor + 1 else currentFloor - 1

  private def updateDestinationsWith(currently: Int) =
    if (atTheRightFloor(currently)) destinations.drop(1) else destinations

  private def goToNextDestinationIfThereIsOne: Elevator = if (destinations.nonEmpty) executeAction() else this
}

object Elevator {
  def apply(currentFloor: Int = 0, destinations: List[Int] = List()): Elevator = {
    val elevator = new Elevator(currentFloor, destinations)
    TickerClock.add(elevator)
    elevator
  }
}
