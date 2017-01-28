package elevator

object ConsoleElevator extends App {
  Elevator()

  val elevatorPrinterThread = new Thread(new ElevatorPrinter)
  val inputHandlerThread = new Thread(new InputHandler)

  elevatorPrinterThread.start()
  inputHandlerThread.start()
}

class ElevatorPrinter extends Runnable {
  override def run(): Unit = {
    while (true) {
      print("\r" + TickerClock.tick() + " --- Where would you like the elevator to go?")
      Thread.sleep(1000)
    }
  }
}

class InputHandler extends Runnable {
  override def run(): Unit = {
    while (true) {
      val line = scala.io.StdIn.readLine()
      val desiredFloor = Integer.parseInt(line)
      Elevator.callAt(desiredFloor)
    }
  }
}
