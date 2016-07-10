package marsrover

case class Rover(val coordinates: (Int, Int), val direction: Direction) {

  private implicit def coordToXY(coord: (Int, Int)): XY = new XY(coord)

  private implicit def XYtoCoord(xy: XY): (Int, Int) = xy.coord

  def location = Location(coordinates, direction)

  def execute(commands: List[Char]): Rover = {
    commands match {
      case x :: xs => run(x).execute(xs)
      case nil => this
    }
  }

  def run(command: Char): Rover = {
    if (command == 'r') {
      new Rover(coordinates, direction.right)
    } else if (command == 'l') {
      new Rover(coordinates, direction.left)
    } else if (command == 'f') {
      new Rover(coordinates + direction.forward, direction)
    } else {
      new Rover(coordinates - direction.forward, direction)
    }
  }
}

abstract class Direction {
  def right: Direction

  def left: Direction

  def forward: (Int, Int)
}

case object North extends Direction {
  override def right: Direction = East

  override def left: Direction = West

  override def forward: (Int, Int) = (0, 1)
}

case object South extends Direction {
  override def right: Direction = West

  override def left: Direction = East

  override def forward: (Int, Int) = (0, -1)
}

case object East extends Direction {
  override def right: Direction = South

  override def left: Direction = North

  override def forward: (Int, Int) = (1, 0)
}

case object West extends Direction {
  override def right: Direction = North

  override def left: Direction = South

  override def forward: (Int, Int) = (-1, 0)
}

case class Location(coord: (Int, Int), direction: Direction)

class DefaultRover extends Rover((0, 0), North)

class XY(val coord: (Int, Int)) {
  def +(other: XY): XY = {
    new XY(coord._1 + other.coord._1, coord._2 + other.coord._2)
  }

  def -(other: XY): XY = {
    new XY(coord._1 - other.coord._1, coord._2 - other.coord._2)
  }
}
