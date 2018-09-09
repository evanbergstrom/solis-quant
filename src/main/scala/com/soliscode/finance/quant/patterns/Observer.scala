package com.soliscode.finance.quant.patterns

import scala.collection.mutable
import scala.util.Failure


object ObservableSettings {
  var updatesEnabled = true
  var updatesDeferred = false
  private val deferredObservers = mutable.Set[Observer]()

  def enableUpdates(): Unit = {
    if (deferredObservers.nonEmpty) {
      val err = Observable.notifyObservers(deferredObservers)
      deferredObservers.clear()
      err match {
        case Failure(e) => throw new RuntimeException(s"could not notify one or more observers", e)
        case _ =>
      }
    }
  }

  def disableUpdates(deferred: Boolean = false): Unit = {
    updatesEnabled = false
    updatesDeferred = deferred
  }

  def registerDeferredObservers(observers: Iterable[Observer]) : Unit =
    if (updatesDeferred) deferredObservers ++= observers

  def unregisterDeferredObserver(observer: Observer): Unit =
    deferredObservers.dropWhile(w => w == observer)
}

trait Observer {
  private val observables = mutable.Set[Observable]()

  def registerWith(observable: Observable): Boolean = {
    observable.registerObserver(this)
    observables.add(observable)
  }

  def registerWithObservables(observer: Observer): Unit =
    observables.foreach(o => o.registerObserver(observer))

  def unregisterWith(observable: Observable): Int = {
    observable.unregisterObserver(this)
    observables -= observable
    observables.size
  }

  def unregisterWithAll(): Unit = {
    observables.foreach(o => o.unregisterObserver(this))
    observables.clear()
  }

  def update(): Unit

  def deepUpdate(): Unit = {
    update()
  }
}

