package models

import org.joda.time._
import play.api.libs.json._

/**
  * Created by felipe on 09/03/17.
  */

case class Operation( operationType: String, operationValue: String, operationDate: String)

object Operation {

  implicit object OperationFormat extends Format[Operation]{

    def reads( json: JsValue): JsResult[Operation] = {
      val operationType  = (json \ "operationType").as[String]
      val operationValue = (json \ "operationValue").as[String]
      val operationDate  = (json \ "operationDate").as[String]

      JsSuccess(Operation(operationType, operationValue, operationDate))
    }

    def writes(operation: Operation): JsValue = {
      val operationAsList = Seq(
        "operationType"  -> JsString(operation.operationType),
        "operationValue" -> JsString(operation.operationValue),
        "operationDate"  -> JsString(operation.operationDate) )

      JsObject(operationAsList)
    }

  }
}
