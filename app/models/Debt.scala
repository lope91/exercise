package models

import org.joda.time.DateTime

/**
  * Created by felipe on 15/03/17.
  */
case class Debt( debtValue: Double, startDate: DateTime, endDate: DateTime) {
  def this( debtValue: Double, startDate: DateTime ) = this(debtValue, startDate, null)
}

