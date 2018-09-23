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

package com.soliscode.finance.quant.time.daycounters

import java.time.Month._
import java.time.{LocalDate, Month}
import java.util.Calendar

import com.soliscode.finance.quant.time.Dates._
import com.soliscode.finance.quant.time.{DayCounter, Schedule}

object ActualActual {
  sealed trait Convention
  object Isma extends Convention
  object Bond extends Convention
  object Isda extends Convention
  object Historical extends Convention
  object Actual365 extends Convention
  object Afb extends Convention
  object Euro extends Convention

  def apply(conv: Convention = Isda, sched: Schedule): DayCounter = conv match {
    case Isma | Bond => new ActualActualIsma(sched)
    case Isda | Historical | Actual365 => new ActualActualIsda()
    case Afb | Euro => new ActualActualAfb()
  }
}

class ActualActualIsma(var schedule: Schedule) extends DayCounter {
  override def name: String = "Actual/Actual (ISMA)"

  override def yearFraction(start: LocalDate, end: LocalDate, refPeriodStart: LocalDate, refPeriodEnd: LocalDate): Double = {
    if (start.isEqual(end))
      0.0
    else if (start.isAfter(end))
      -yearFraction(end, start, refPeriodStart, refPeriodEnd)
    else {
      // estimate roughly the length in months of a period
      val refMonths: Int = (0.5 + 12 * refPeriodStart.dayCountUntil(refPeriodEnd)/365.0).toInt

      val (refStart, refEnd, months) =
        if (refMonths > 0) (refPeriodStart, refPeriodEnd, refMonths) else (refPeriodStart, refPeriodStart.plusYears(1), 12)

      val period = months/12.0

      if (end.isNotAfter(refEnd)) {
        // here refPeriodEnd is a future (notional?) payment date
        if (start.isNotBefore(refStart)) {
          // here refPeriodStart is the last (maybe notional) payment date refPeriodStart <= d1 <= d2 <= refPeriodEnd
          // [maybe the equality should be enforced, since refPeriodStart < d1 <= d2 < refPeriodEnd could give wrong
          // results] ???
          period * start.dayCountUntil(end).toDouble / refStart.dayCountUntil(refEnd).toDouble
        } else {
          // here refPeriodStart is the next (maybe notional) payment date and refPeriodEnd is the second next
          // (maybe notional) payment date.
          // d1 < refPeriodStart < refPeriodEnd AND d2 <= refPeriodEnd this case is long first coupon

          // the last notional payment date
          val previousRef : LocalDate = if (schedule.dates.isEmpty) {
            refStart.minusMonths(months)
          } else {
            schedule.calendar.advance(refStart, schedule.tenor.negated(), schedule.convention, schedule.endOfMonth)
          }

          if (end.isAfter(refPeriodStart))
            yearFraction(start, refStart, previousRef, refStart) + yearFraction(refStart, end, refStart, refEnd)
          else
            yearFraction(start, end, previousRef,refStart)
        }
      } else {
        // here refPeriodEnd is the last (notional?) payment date. d1 < refPeriodEnd < d2 AND refPeriodStart < refPeriodEnd
        require(refStart.isBefore(start) || refStart.isEqual(start), "invalid dates: d1 < refPeriodStart < refPeriodEnd < d2")
        // now it is: refPeriodStart <= d1 < refPeriodEnd < d2

        // the part from d1 to refPeriodEnd
        var sum = yearFraction(start, refEnd, refStart, refEnd)

        // the part from refPeriodEnd to end count how many regular periods are in [refPeriodEnd, d2], then add the
        // remaining time
        var i=0
        var newRefStart = refEnd.plusMonths(months*i)
        var newRefEnd = refEnd.plusMonths(months*(i+1))
        while(!end.isBefore(newRefEnd)) {
          sum += period
          i += 1
          newRefStart = refEnd.plusMonths(months*i)
          newRefEnd = refEnd.plusMonths(months*(i+1))
        }
        sum += yearFraction(newRefStart,end,newRefStart,newRefEnd)
        sum
      }
    }
  }
}

class ActualActualIsda extends DayCounter {
  override def name: String = "Actual/Actual (ISDA)"

  override def yearFraction(start: LocalDate, end: LocalDate, refPeriodStart: LocalDate, refPeriodEnd: LocalDate): Double =
    if (start.isEqual(end)) {
      0.0
    } else if (start.isAfter(end)) {
      -yearFraction(end, start, start, end)
    } else {
      val (startYear, endYear) = (start.getYear, end.getYear)
      val daysInStartYear = if (start.isLeapYear) 366.0 else  365.0
      val daysInEndYear = if (end.isLeapYear) 366.0 else 365.0

      val daysInWholeYears : Double = endYear - startYear - 1
      val daysInFirstYear = start.dayCountUntil(LocalDate.of(startYear + 1, JANUARY, 1)).toDouble / daysInStartYear
      val daysInLastYEar = LocalDate.of(endYear, 1, 1).dayCountUntil(end).toDouble / daysInEndYear

      daysInWholeYears + daysInFirstYear + daysInLastYEar
    }
}

class ActualActualAfb extends DayCounter {
  override def name: String = "Actual/Actual (AFB)"

  override def yearFraction(start: LocalDate, end: LocalDate, refPeriodStart: LocalDate, refPeriodEnd: LocalDate): Double =
    if (start.isEqual(end)) {
      0.0
    } else if (start.isAfter(end)) {
      -yearFraction(end, start, end, start)
    } else {
      var newEnd, temp = end
      var sum = 0.0
      while (temp.isAfter(start)) {
        temp = newEnd.minusYears(1)
        if (temp.getDayOfMonth == 28 && temp.getMonth == Month.FEBRUARY && temp.isLeapYear) {
          temp.plusDays(1)
        }
        if (temp.isEqual(start) || temp.isAfter(start)) {
          sum += 1.0
          newEnd = temp
        }
      }

      var den = 365.0

      if (newEnd.isLeapYear) {
        temp = LocalDate.of(newEnd.getYear, Month.FEBRUARY, 29)
        if (newEnd.isAfter(temp) && start.isNotAfter(end))
          den += 1.0
      } else if (start.isLeapYear) {
        temp = LocalDate.of(29, Calendar.FEBRUARY, start.getYear)
        if (newEnd.isAfter(temp) && start.isNotAfter(temp))
          den += 1.0
      }

      sum + (start.dayCountUntil(newEnd) / den)
    }
}
