package com.soliscode.finance.quant.time

import java.time.{LocalDate, Period}

import com.soliscode.finance.quant.time.BusinessDayConvention.Following
import com.soliscode.finance.quant.time.calendars.UnitedStates
import com.soliscode.finance.quant.time.calendars.UnitedStates.GovernmentBond

import scala.collection.mutable.ListBuffer

object Schedule {
  def apply(dates: Array[LocalDate]) : Schedule = ???

  def apply(): Schedule =
    new Schedule(UnitedStates.withMarket(GovernmentBond), Following, Following, null, true, LocalDate.now(),
      LocalDate.now(), ListBuffer[LocalDate](), ListBuffer[Boolean](), DateGeneration.Forward)

  def apply(effectiveDate: LocalDate,
            terminationDate: LocalDate,
            tenor: Period,
            calendar: Calendar,
            convention: BusinessDayConvention,
            terminationConvention: BusinessDayConvention,
            rule: DateGeneration.Rule,
            endOfMonth: Boolean,
            firstDate: Option[LocalDate] = None,
            lastDate: Option[LocalDate] = None) : Unit = {

  }
}

class Schedule(val calendar: Calendar,
               val convention: BusinessDayConvention,
               val terminationConvention: BusinessDayConvention,
               val tenor: Period,
               val endOfMonth: Boolean,
               val firstDate: LocalDate,
               val lastDate: LocalDate,
               val dates: ListBuffer[LocalDate],
               val isRegular: ListBuffer[Boolean],
               val rule: DateGeneration.Rule) {

  def previousDate(date: LocalDate): LocalDate = ???
  def nextDate(date: LocalDate): LocalDate = ???
  def hasIsRegular: Boolean = ???


  //  def tenor: Period = {
  //    require(this.tenor.isDefined, "full interface (tenor) not available")
  //    tenor.get
  //  }
}

