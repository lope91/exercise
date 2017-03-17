package models



import org.joda.time.{DateTime, LocalDate}
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import play.api.libs.json._

/**
  * Created by felipe on 09/03/17.
  */

case class Operation( description: String, operationType: String, operationBalance: Double, operationDate: DateTime)

object Operation {

  implicit object OperationFormat extends Format[Operation]{

    def reads( json: JsValue): JsResult[Operation] = {
      var operationType  = (json \ "operationType").as[String]
      var operationValue = (json \ "operationBalance").as[Double]
      var description = (json \ "operationDescription").as[String]

      var formatter: DateTimeFormatter  = DateTimeFormat.forPattern("dd/MM/yyyy")
      var operationDate: DateTime       = formatter.parseDateTime((json \ "operationDate").as[String])

      JsSuccess(Operation(description, operationType, operationValue, operationDate))
    }

    def writes(operation: Operation): JsValue = {
      var operationAsList = Seq(
        "description"    -> JsString(operation.description),
        "operationType"  -> JsString(operation.operationType),
        "operationValue" -> JsNumber(operation.operationBalance),
        "operationDate"  -> JsString(operation.operationDate.toString()) )

      JsObject(operationAsList)
    }
  }

}