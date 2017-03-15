package models

import java.util

import play.api.libs.json._

/**
  * Created by felipe on 09/03/17.
  */
case class Account(currentBalance: Double, operations: util.ArrayList[Operation]){
  def operate(operationType: String, value: Double): Account = operationType match {
    case "C" => this.copy(this.currentBalance.+(value), this.operations)
    case "D" => this.copy(this.currentBalance.-(value), this.operations)
  }
}

object Account {

  implicit object AccountFormat extends Format[Account]{

    def reads(json: JsValue): JsResult[Account] = {

      var operations             = (json).as[Operation]
      var arrayOperations: util.ArrayList[Operation] = new util.ArrayList[Operation]()
      arrayOperations.add(operations)

      var operationValue: Double = 0
      if(operations.operationType.equals("C")){
        operationValue = operationValue + operations.operationBalance
      } else {
        operationValue = operationValue - operations.operationBalance
      }

      JsSuccess(Account(operationValue, arrayOperations))

    }

    def writes(account: Account): JsValue = {
      var accountAsList = Seq("currentBalance" -> JsString(account.currentBalance.toString()),
                              "operations"     -> JsString(account.operations.toString()))

      JsObject(accountAsList)
    }

  }


}
