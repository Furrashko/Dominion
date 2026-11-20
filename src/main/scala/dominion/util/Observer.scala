package dominion.util

trait Observer[T] {
  def update(value: T): Unit
}

/**
 * Basic observable implementation.
 * Observer registration is encapsulated so callers cannot modify the set directly.
 */
trait Observable[T] {

  private var registeredObservers: Set[Observer[T]] = Set.empty

  def addObserver(observer: Observer[T]): Unit =
    registeredObservers += observer

  def removeObserver(observer: Observer[T]): Unit =
    registeredObservers -= observer

  def notifyObservers(value: T): Unit =
    registeredObservers.foreach(_.update(value))
}
