package controllers

import java.text.DecimalFormat
import java.util
import java.util.{Collections, Comparator}
import javax.inject._

import play.api.mvc._
import models.{Account, Debt, Operation}
import org.joda.time.{DateTime, Interval}
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import play.api.libs.json._

import scala.collection.mutable

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject() extends Controller {

  // Initialize the "pool" of accounts in bank
  var accounts = new mutable.HashMap[String, Account]()

  /*
  First step: adding the operations on that checking account

  Create a HTTP service in which you can add an operation to a given checking
  account, identified by the account number. This operation will contain the
  account number, a short description, a amount and the date it happened. Keep
  in mind you have to support both credit and debit operations, i.e, both
  putting and taking money out of the account.
  */
  def saveTransaction =  Action { request =>
    /* Receiving the account operation via JSON */
    val json = request.body.asJson.get

    /* Getting account number from the json received */
    val accountNumber = (json \ "number").as[String]

    /* if the account is already created in the bank, we will not create it again.
     * We will just update the account balance.
     */
    if (accounts.contains(accountNumber)) {

      // Convert the payload into an Operation object
      val operation        = json.as[Operation]
      // Get the currentAccount to capture some informations
      val currentAccount   = accounts.get(accountNumber)

      // If the operation date is after now,
      // the current balance will not be updated
      if(operation.operationDate.isAfterNow) {
        accounts.put(
          accountNumber, currentAccount.get
        )
      } else {
        try {
          accounts.put(
            accountNumber, currentAccount.get.updateBalance(
              operation.operationType,
              operation.operationBalance
            )
          )
        } catch {
          case e: Exception => Ok(Json.toJson(Map("Error" -> e.getMessage)))
        }

      }

      // Add the operation in the account
      accounts(accountNumber).operations.add(operation)
      // Sorting the array by the operation date
      // to be easier to get information in other services
      accounts(accountNumber).operations.sort( new Comparator[Operation] {
        override def compare(o1: Operation, o2: Operation) = {
          o1.operationDate.getMillis.compare(o2.operationDate.getMillis)
        }
      })

      // Respond in payload format
      Ok(Json.toJson(Map("Message" -> "Transaction completed successfully")))

    } else {
      // Creating the Account
      try {
        accounts.put(accountNumber, json.as[Account])
      } catch {
        // If the operation isn't of a known type, the exception is raised
        case e: Exception => Ok(Json.toJson(Map("Error" -> e.getMessage)))
      }


      // Respond in payload format
      Ok(Json.toJson(Map("Message" -> "Account created and transaction completed successfully")))
    }

    Ok
  }

  def getCurrentBalance(accountNumber: String) =  Action { request =>

    // Verify if the account exists
    if (accounts.contains(accountNumber)) {
      // get the current balance
      val currentBalance:Double = accounts(accountNumber).currentBalance
      // Respond in payload format
      Ok(
        Json.toJson(Map[String, Double]("Balance" -> currentBalance))
      )
    } else {
      // Respond in payload format
      Ok(
        Json.toJson(Map[String, String]("Error" -> "Unknown bank account"))
      )
    }

  }

  /*
  Third step: Get the bank statement

  Create a HTTP endpoint which returns the bank statement of a period of dates.
  This statement will contain the operations of each day and the balance at theBal
  end of each day.
  */
  def getStatement =  Action { request =>

    val accountNumber   = request.getQueryString("accountNumber").get
    val startDate       = request.getQueryString("startDate").get
    val finalDate       = request.getQueryString("finalDate").get

    if (accounts.contains(accountNumber)) {

      val formatter: DateTimeFormatter  = DateTimeFormat.forPattern("dd/MM/yyyy")
      val startDateFormatted: DateTime  = formatter.parseDateTime(startDate)
      val finalDateFormatted: DateTime  = formatter.parseDateTime(finalDate)
      val interval: Interval            = new Interval(startDateFormatted, finalDateFormatted)
      var mapStatement: Seq[JsValue] = Seq[JsValue]()
      var currentAccount = accounts.get(accountNumber)
      var balance: Double = 0

      for(i <- 0 until currentAccount.get.operations.size()) {

        balance = currentAccount.get.operate(
          balance, currentAccount.get.operations.get(i).operationBalance,
          currentAccount.get.operations.get(i).operationType
        )

        if(interval.contains(currentAccount.get.operations.get(i).operationDate)){

          mapStatement = mapStatement.:+(Json.toJson(Map(
            "Transaction Date" -> Json.toJson(currentAccount.get.operations.get(i).operationDate),
            "Operation Type" -> Json.toJson(currentAccount.get.getFullDescription(currentAccount.get.operations.get(i).operationType)),
            "Transaction Description" -> Json.toJson(currentAccount.get.operations.get(i).description),
            "Transaction Value" -> Json.toJson(currentAccount.get.operations.get(i).operationBalance),
            "Balance" -> Json.toJson(balance)
          )))

        }
      }

      Ok(Json.toJson(Map("Statement" -> mapStatement)))

    } else {
      Ok(Json.toJson(Map("Error" -> "Unknown Bank account")))
    }

  }

   /*
   Fourth step:

   Compute periods of debt.  Create a HTTP endpoint which returns the periods which the account's balance
   was negative, i.e, periods when the bank can charge interest on that account.
   */
  def getPeriodsOfDebt(accountNumber: String) = Action { request =>

    // Evaluate if the account really exists
    if (accounts.contains(accountNumber)) {

      // Initialize variables
      val currentAccount = accounts.get(accountNumber)
      var currentBalance: Double = 0
      var debtValue: Double = 0
      var debtStartDate: DateTime = null
      var debtEndDate: DateTime = null
      var status: String = ""
      val formatDecimal: DecimalFormat = new DecimalFormat("0.##")
      var mapDebts: Seq[JsValue] = Seq[JsValue]()

      // Check all transactions made for that account
      for (i <- 0 until currentAccount.get.operations.size()) {

        /*
        Here, will be verified whether the transaction analyzed
        left the account with negative balance or not.
         */
        currentAccount.get.operations.get(i).operationType match {
          case "DEP" | "SAL" | "CRE" => currentBalance += currentAccount.get.operations.get(i).operationBalance
          case "PUR" | "WIT" | "DEB" => currentBalance -= currentAccount.get.operations.get(i).operationBalance
        }

        // If you left negative...
        if (currentBalance < 0) {
          status = "N" // Status N -> Negative
          debtValue = currentBalance * -1 // To get a correct presentation to payload
          if (debtStartDate == null) debtStartDate = currentAccount.get.operations.get(i).operationDate
        } else {
        // If you left postive (or equals to zero)...
          if (status.equals("N")) {
            debtEndDate = currentAccount.get.operations.get(i).operationDate
            // Add a debt that will be in the payload
            mapDebts = mapDebts.:+(Json.toJson(Map(
              "Principal" -> Json.toJson(formatDecimal.format(debtValue)),
              "Start" -> Json.toJson(debtStartDate.toString()),
              "End" -> Json.toJson(debtEndDate.toString())
            )))
          }

          status = "P" // Status P -> Positive
          debtStartDate = null
        }

      }

      // If the last evaluated operation has left the negative account,
      // it should be presented in the payload without the final day on which it was negative
      if (status.equals("N")) {
        // Add a debt that will be in the payload
        mapDebts = mapDebts.:+(Json.toJson(Map(
          "Principal" -> Json.toJson(debtValue),
          "Start" -> Json.toJson(debtStartDate.toString())
        )))
      }

      // Response the payload
      Ok(
        Json.toJson( Map("Debts" -> mapDebts) )
      )

    } else {
      // Account unknown (response in payload format)
      Ok(
        Json.toJson(Map("Error" -> "Unknown bank account"))
      )
    }
  }

}

