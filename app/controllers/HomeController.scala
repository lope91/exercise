package controllers

import java.text.DecimalFormat
import java.util
import java.util.{Collections, Comparator}
import javax.inject._

import play.api.mvc._
import models.{Account, Operation}
import org.joda.time.{DateTime, Interval}
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}

import scala.collection.mutable

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject() extends Controller {

  var accounters = new mutable.HashMap[String, Account]()

  def saveTransaction =  Action { request =>
    /* Receiving the account operation via JSON */
    var json = request.body.asJson.get

    /* Getting account number from the json received */
    var accountNumber = (json \ "number").as[String]

    /* if the account is already created in the bank, we will not create it again.
     * We will just update the account balance.
     */
    if (accounters.contains(accountNumber)) {

      // Convert the payload into an Operation object
      var operation        = json.as[Operation]
      // Get the currentAccount to capture some informations
      var currentAccount   = accounters.get(accountNumber)

      // Update the current balance of account
      accounters.put(
          accountNumber, currentAccount.get.operate(
            operation.operationType,
            operation.operationBalance
          )
      )

      // Add the operation in the account
      accounters.get(accountNumber).get.operations.add(operation)
      // Sorting the array by the operation date
      accounters.get(accountNumber).get.operations.sort( new Comparator[Operation] {
        override def compare(o1: Operation, o2: Operation) = {
          o1.operationDate.getMillis.compare(o2.operationDate.getMillis)
        }
      })

    } else {
      /* Creating the Account*/
      var account = json.as[Account]
      accounters.put(accountNumber, account)
    }


    Ok
  }


  def getCurrentBalance(accountNumber: String) =  Action { request =>

    if (accounters.contains(accountNumber)) {
      var currentBalance:Double = accounters.get(accountNumber).get.currentBalance
      Ok(s"Saldo Atual: $currentBalance \n")
    } else {
      Ok("Unknown Bank account")
    }

  }

  def getStatement =  Action { request =>

    val accountNumber   = request.getQueryString("accountNumber").get
    val startDate       = request.getQueryString("startDate").get
    val finalDate       = request.getQueryString("finalDate").get
    var message: String = ""

    if (accounters.contains(accountNumber)) {

      val formatter: DateTimeFormatter  = DateTimeFormat.forPattern("dd/MM/yyyy")
      val startDateFormatted: DateTime  = formatter.parseDateTime(startDate)
      val finalDateFormatted: DateTime  = formatter.parseDateTime(finalDate)
      val interval: Interval            = new Interval(startDateFormatted, finalDateFormatted)

      val currentAccount = accounters.get(accountNumber)

      for(i <- 0 to currentAccount.get.operations.size() - 1) {
        if(interval.contains(currentAccount.get.operations.get(i).operationDate)){
          message += "Data da operação: %s | Tipo da Operação: %s | Valor da Operação %f. \n".format(
            currentAccount.get.operations.get(i).operationDate.toString(),
            currentAccount.get.operations.get(i).operationType,
            currentAccount.get.operations.get(i).operationBalance)
        }
      }

      Ok(message)

    } else {
      Ok("Unknown Bank account")
    }

    Ok
  }

  def getPeriodsOfDebt(accountNumber: String) = Action { request =>

    if (accounters.contains(accountNumber)) {

      val currentAccount          = accounters.get(accountNumber)
      var currentBalance: Double  = 0
      var debtValue: Double       = 0
      var debtStartDate: DateTime = null
      var debtEndDate: DateTime   = null
      var status: String          = ""
      var msg: String             = ""
      val formatDecimal: DecimalFormat = new DecimalFormat("0.##")


      for(i <- 0 to currentAccount.get.operations.size() - 1) {

        currentAccount.get.operations.get(i).operationType match {
          case "C" => currentBalance += currentAccount.get.operations.get(i).operationBalance
          case "D" => currentBalance -= currentAccount.get.operations.get(i).operationBalance
        }

        if(currentBalance < 0) {
          status         = "N"
          debtValue      = currentBalance * -1
          if(debtStartDate == null) debtStartDate  = currentAccount.get.operations.get(i).operationDate

        } else {
          if (status.equals("N")) {
            debtEndDate = currentAccount.get.operations.get(i).operationDate
            println("Valor devido: %f \nData de Inicio: %s \nData Final: %s \n\n".format(formatDecimal.format(debtValue), debtStartDate.toString(), debtEndDate.toString()))
          }
          status = "P"
          debtStartDate = null
        }

      }

      if(status.equals("N")){
        println("Valor devido: %f \nData de Inicio: %s \n\n".format(formatDecimal.format(debtValue)., debtStartDate.toString()))
      }

    } else {
      Ok("Unknown Bank account")
    }

    Ok
  }

}

