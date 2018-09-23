/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl
 Copyright (C) 2018 Evan Bergstrom

 This file is provided under the BSD open software license. This is a port of QuantLib,
 a free-software/open-source library for financial quantitative analysts and developers
 (http://quantlib.org/) to Scala. The basic structure and design of the library has been
 preserved, but the naming conventions, types, collection classes and implementation
 have been modified to support common Scala idioms.

 See the full license in the license file (LICENSE.txt)
*/

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
