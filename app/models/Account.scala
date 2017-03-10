package models

import play.api.libs.json._

/**
  * Created by felipe on 09/03/17.
  */
case class Account(number: String, description: String, operations: Array[Operation])

object Account {

  implicit object AccountFormat extends Format[Account]{

    def reads( json: JsValue): JsResult[Account] = {
      val number      = (json \ "number").as[String]
      val description = (json \ "description").as[String]
      val operations  = (json \ "operations").as[Array[Operation]]

      JsSuccess(Account(number, description, operations))
    }

    def writes(account: Account): JsValue = {
      val accountAsList = Seq("number"      -> JsString(account.number),
                              "description" -> JsString(account.description),
                              "operations"  -> JsString(account.operations.toString()))
      JsObject(accountAsList)
    }

  }
}