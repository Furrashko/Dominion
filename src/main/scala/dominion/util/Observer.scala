package dominion.util

trait Observer[T] {
  def update(value: T): Unit
}

trait Observable[T]:
  var observers: Set[Observer[T]] = Set.empty

  def addObserver(observer: Observer[T]): Unit = {
    observers += observer
  }
  def removeObserver(observer: Observer[T]): Unit = {
    observers -= observer
  }

  def notifyObservers(value: T): Unit = {
    observers.foreach(_.update(value))
  }