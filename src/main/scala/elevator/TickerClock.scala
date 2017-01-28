package elevator

object TickerClock {
  private var entity: TimedEntity = _

  def add(entity: TimedEntity): Unit = this.entity = entity

  def tick(): TimedEntity = entity.executeAction()

  def getCurrent: TimedEntity = entity
}

trait TimedEntity {
  def executeAction(): TimedEntity
}
