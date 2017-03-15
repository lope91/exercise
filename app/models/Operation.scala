package models



import org.joda.time.{DateTime, LocalDate}
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import play.api.libs.json._

/**
  * Created by felipe on 09/03/17.
  */

case class Operation( operationType: String, operationBalance: Double, operationDate: DateTime)

object Operation {

  implicit object OperationFormat extends Format[Operation]{

    def reads( json: JsValue): JsResult[Operation] = {
      var operationType  = (json \ "operationType").as[String]
      var operationValue = (json \ "operationBalance").as[Double]

      var formatter: DateTimeFormatter  = DateTimeFormat.forPattern("dd/MM/yyyy")
      var operationDate: DateTime       = formatter.parseDateTime((json \ "operationDate").as[String])

      JsSuccess(Operation(operationType, operationValue, operationDate))
    }

    def writes(operation: Operation): JsValue = {
      var operationAsList = Seq(
        "operationType"  -> JsString(operation.operationType),
        "operationValue" -> JsNumber(operation.operationBalance),
        "operationDate"  -> JsString(operation.operationDate.toString()) )

      JsObject(operationAsList)
    }
  }

}