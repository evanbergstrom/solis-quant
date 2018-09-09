package com.soliscode.finance.quant.time

object DateGeneration {

  sealed trait Rule

  /** Backward from termination date to effective date. */
  object Backward extends Rule

  /** // Forward from effective date to termination date. */
  object Forward extends Rule

  /** // No intermediate dates between effective date and termination date. */
  object Zero extends Rule

  /** All dates but effective date and termination date are taken to be on the third  wednesday */
  object ThirdWednesday extends Rule

  /** All dates but the effective date are taken to be the twentieth of their month (used for CDS schedules in
    * emerging markets.)  The termination date is also modified.  */
  object Twentieth extends Rule

  /** Same as TwentiethIMM with unrestricted date ends and log/short stub coupon period (old CDS convention). */
  object OldCDS extends Rule

  /** Credit derivatives standard rule since 'Big Bang' changes in 2009. */
  object CDS extends Rule

  /** Credit derivatives standard rule since December 20th, 2015. */
  object CDS2015 extends Rule
}

