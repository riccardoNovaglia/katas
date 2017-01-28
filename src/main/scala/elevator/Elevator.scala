package elevator

import scala.collection.mutable.ListBuffer

class Elevator(var currentFloor: Int = 0) extends TimedEntity {
  private var destinations = ListBuffer[Int]()

  def goTo(floor: Int): Elevator = {
    destinations += floor
    this
  }

  override def executeAction(): Unit = {
    if (!atTheRightFloor) {
      currentFloor = if (destinations.head > currentFloor) currentFloor + 1 else currentFloor - 1
      checkDestinationReached()
    } else {
      goToNextDestinationIfThereIsOne()
    }
  }

  private def atTheRightFloor = destinations.isEmpty || destinations.head == currentFloor

  private def checkDestinationReached() = if (atTheRightFloor) destinations = destinations.drop(1)

  private def goToNextDestinationIfThereIsOne() = if (destinations.nonEmpty) executeAction()
}

object Elevator {
  def apply(currentFloor: Int = 0): Elevator = {
    val elevator = new Elevator(currentFloor)
    TickerClock.add(elevator)
    elevator
  }
}
