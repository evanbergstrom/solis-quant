package com.soliscode.finance.quant.patterns

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object Observable {
  def notifyObservers(observers: Iterable[Observer]): Try[Unit] = {
    var error : Try[Unit] = Success()
    observers.foreach(o => Try(o.update()) match {
      case Failure(e) => error = Failure(e)
      case Success(u) =>
    })
    error
  }
}

trait Observable {
  private val observers = mutable.Set[Observer]()

  def notifyObservers() : Unit = {
    if (!ObservableSettings.updatesEnabled) {
      ObservableSettings.registerDeferredObservers(observers)
    } else if (observers.nonEmpty) {
      Observable.notifyObservers(observers).foreach(e => new RuntimeException("Some observers could not be updates"))
    }
  }

  def registerObserver(observer: Observer) : Unit = {
    observers += observer
  }

  def unregisterObserver(observer: Observer): Unit = {
    if (ObservableSettings.updatesDeferred)
      ObservableSettings.unregisterDeferredObserver(observer)

    observers -= observer
  }
}
