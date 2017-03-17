package models

import java.util

import play.api.libs.json._
import com.google.gson._


/**
  * Created by felipe on 09/03/17.
  */
case class Account(currentBalance: Double = 0, operations: util.ArrayList[Operation]){
  def updateBalance(operationType: String, value: Double): Account = operationType match {
    case "DEP" | "SAL" | "CRE" => this.copy(this.currentBalance.+(value), this.operations)
    case "PUR" | "WIT" | "DEB" => this.copy(this.currentBalance.-(value), this.operations)
    case _ => throw new Exception("Unknown transaction type for this account.")
  }

  def getFullDescription(operationType: String): String = operationType match {
    case "DEP" => "Deposit"
    case "SAL" => "Salary"
    case "CRE" => "Credit"
    case "PUR" => "Purchase"
    case "WIT" => "Withdrawal"
    case "DEB" => "Debit"
  }

  def operate( balanceValue: Double, transactionValue: Double, operationType: String): Double = operationType match {
    case "DEP" | "SAL" | "CRE" => balanceValue.+(transactionValue)
    case "PUR" | "WIT" | "DEB" => balanceValue.-(transactionValue)
    case _ => throw new Exception("Unknown transaction type for this account.")
  }
}

object Account {

  implicit object AccountFormat extends Format[Account]{

    def reads(json: JsValue): JsResult[Account] = {

      var operations = json.as[Operation]
      var arrayOperations: util.ArrayList[Operation] = new util.ArrayList[Operation]()
      arrayOperations.add(operations)

      var acc: Account = Account(0, arrayOperations)
      try {
        acc = acc.updateBalance(operations.operationType, operations.operationBalance)
      } catch {
        case e: Exception => throw new Exception(e.getMessage)
      }

      JsSuccess(acc)
    }

    def writes(account: Account): JsValue = {

      var accountAsList = Seq("currentBalance" -> JsNumber(account.currentBalance),
                              "operations"     ->  JsString(new Gson().toJson(account.operations)))

      JsObject(accountAsList)
    }

  }

}
