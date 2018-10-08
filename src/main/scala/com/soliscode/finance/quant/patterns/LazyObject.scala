/*
 *  Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 *  Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl
 *  Copyright (C) 2018 Evan Bergstrom
 *
 * This file is provided under the BSD open software licemnse. This is a port of QuantLib, a free-software/open-source library for financial quantitative analysts and developers (http://quantlib.org/) to Scala. The basic structure and design of the library has been preserved, but thebnaming conventions, basic types, collection classes and implementation have been modified to support common Scala idioms.
 *
 * See the full license in the license file (LICENSE.txt)
 */

package com.soliscode.finance.quant.patterns

trait LazyObject extends Observable with Observer {

  protected var calculated: Boolean = false
  private var frozen: Boolean = false
  private var alwaysForward: Boolean = false

  def update(): Unit = {
    // forwards notifications only the first time// forwards notifications only the first time

    if (calculated || alwaysForward) {
      // set to false early
      //    1) to prevent infinite recursion
      //    2) otherways non-lazy observers would be served obsolete data because of calculated_ being still true
      calculated = false

      // observers don't expect notifications from frozen objects
      if (!frozen) notifyObservers()

      // exiting notifyObservers() calculated_ could be
      // already true because of non-lazy observers
    }
  }

  /**
    * This method force the recalculation of any results which would otherwise be cached.
    *
    * @note Explicit invocation of this method is <b>not</b> necessary if the object registered itself as observer
    *       with the structures on which such results depend.  It is strongly advised to follow this policy when
    *       possible.
    */
  def recalculate(): Unit = {
    val wasFrozen = frozen
    calculated = false
    frozen = false
    try {
      calculate()
    } catch {
      case ex: Exception =>
        frozen = wasFrozen
        notifyObservers()
        throw ex
    }
    frozen = wasFrozen
    notifyObservers()
  }

  /**
    * This method constrains the object to return the presently cached results on successive invocations, even if
    * arguments upon which they depend should change.
    */
  def freeze(): Unit = frozen = true

  /**
    * This method reverts the effect of the [[freeze]] method, thus re-enabling recalculations.
    */
  def unfreeze(): Unit =
    // send notifications, just in case we lost any,
    // but only once, i.e. if it was frozen
    if (frozen) {
      frozen = false
      notifyObservers()
    }

  /**
    * This method causes the object to forward all notifications, even when not calculated.  The default
    * behavior is to forward the first notification received, and discard the others until recalculated; the rationale
    * is that observers were already notified, and don't need further notification until they recalculate, at which
    * point this object would be recalculated too.  After recalculation, this object would again forward the first
    * notification received.
    *
    * @note Forwarding all notifications will cause a performance hit, and should be used only when discarding
    *       notifications cause an incorrect behavior.
    */
  def alwaysForwardNotifications(): Unit = alwaysForward = true

  /**
    * This method performs all needed calculations by calling the [[performCalculations]] method.
    *
    * @note Objects cache the results of the previous calculation. Such results will be returned upon later
    *       invocations of [[calculate]]. When the results depend on arguments which could change between invocations,
    *       the lazy object must register itself as observer of such objects for the calculations to be performed
    *       again when they change.
    *
    * @note Should this method be redefined in derived classes, LazyObject::calculate() should be called in the
    *       overriding method.
    */
  protected def calculate(): Unit = {
    if (!calculated && !frozen) {
      calculated = true;   // prevent infinite recursion in case of bootstrapping
      try {
        performCalculations()
      } catch {
        case ex: Exception =>
          calculated = false
          throw ex
      }
    }
  }

  /**
    * This method must implement any calculations which must be (re)done in order to calculate the desired results.
    */
  protected def performCalculations(): Unit



}
