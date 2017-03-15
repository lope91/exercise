package controllers

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

    var accountNumber   = request.getQueryString("accountNumber").get
    var startDate       = request.getQueryString("startDate").get
    var finalDate       = request.getQueryString("finalDate").get
    var message: String = ""

    if (accounters.contains(accountNumber)) {

      var formatter: DateTimeFormatter  = DateTimeFormat.forPattern("dd/MM/yyyy")
      var startDateFormatted: DateTime  = formatter.parseDateTime(startDate)
      var finalDateFormatted: DateTime  = formatter.parseDateTime(finalDate)
      var interval: Interval            = new Interval(startDateFormatted, finalDateFormatted)

      var currentAccount = accounters.get(accountNumber)

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

}

