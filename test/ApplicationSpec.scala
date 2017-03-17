import controllers.HomeController
import org.scalatestplus.play._
import play.api.Application
import play.api.libs.json.Json
import play.api.test._
import play.api.test.Helpers._

import scala.collection.immutable.Stream.Empty

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
class ApplicationSpec extends PlaySpec with OneAppPerTest {

  "Routes" should {

    "send 404 on a bad request" in  {
      route(app, FakeRequest(GET, "/boum")).map(status(_)) mustBe Some(NOT_FOUND)
    }

  }

  "HomeController" should {

    "Insert a transaction successfully" in {

      val control = new HomeController

      val result = control.saveTransaction.apply(
        FakeRequest().
          withHeaders(CONTENT_TYPE->"application/json").withJsonBody(
          Json.toJson(Map(
            "number" -> Json.toJson("999999"),
            "operationDescription" -> Json.toJson("Test Transaction"),
            "operationType" -> Json.toJson("CRE"),
            "operationBalance" -> Json.toJson(10.0),
            "operationDate" -> Json.toJson("15/03/2017")
          ))
        )
      )

      status(result) mustBe OK
    }

    "Get statement successfully" in {

      val control = new HomeController

      val resultTransaction1 = control.saveTransaction.apply(
        FakeRequest().
          withHeaders(CONTENT_TYPE->"application/json").withJsonBody(
          Json.toJson(Map(
            "number" -> Json.toJson("999999"),
            "operationDescription" -> Json.toJson("Test Transaction 1"),
            "operationType" -> Json.toJson("DEB"),
            "operationBalance" -> Json.toJson(10.0),
            "operationDate" -> Json.toJson("15/03/2017")
          ))
        )
      )

      val resultTransaction2 = control.saveTransaction().apply(
        FakeRequest().
          withHeaders(CONTENT_TYPE->"application/json").withJsonBody(
          Json.toJson(Map(
            "number" -> Json.toJson("999999"),
            "operationDescription" -> Json.toJson("Test Transaction 2"),
            "operationType" -> Json.toJson("CRE"),
            "operationBalance" -> Json.toJson(10.0),
            "operationDate" -> Json.toJson("15/03/2017")
          ))
        )
      )

      val resultStatement = control.getStatement().apply(
        FakeRequest(GET, "/getStatement?accountNumber=999999&startDate=10/03/2017&finalDate=16/03/2017")
      )

      status(resultTransaction1) mustBe OK
      status(resultTransaction2) mustBe OK
      status(resultStatement)    mustBe OK
    }

    "Get current balance successfully" in {

      val control = new HomeController

      val resultTransaction1 = control.saveTransaction.apply(
        FakeRequest().
          withHeaders(CONTENT_TYPE->"application/json").withJsonBody(
          Json.toJson(Map(
            "number" -> Json.toJson("999999"),
            "operationDescription" -> Json.toJson("Test Transaction 1"),
            "operationType" -> Json.toJson("DEB"),
            "operationBalance" -> Json.toJson(10.0),
            "operationDate" -> Json.toJson("15/03/2017")
          ))
        )
      )

      val resultTransaction2 = control.saveTransaction().apply(
        FakeRequest().
          withHeaders(CONTENT_TYPE->"application/json").withJsonBody(
          Json.toJson(Map(
            "number" -> Json.toJson("999999"),
            "operationDescription" -> Json.toJson("Test Transaction 2"),
            "operationType" -> Json.toJson("CRE"),
            "operationBalance" -> Json.toJson(10.0),
            "operationDate" -> Json.toJson("15/03/2017")
          ))
        )
      )

      val resultBalance = control.getCurrentBalance("999999").apply(
        FakeRequest(GET, "/getCurrentBalance/999999")
      )

      status(resultTransaction1) mustBe OK
      status(resultTransaction2) mustBe OK
      status(resultBalance)      mustBe OK
    }

    "Get currents debts successfully" in {

      val control = new HomeController

      val resultTransaction1 = control.saveTransaction.apply(
        FakeRequest().
          withHeaders(CONTENT_TYPE->"application/json").withJsonBody(
          Json.toJson(Map(
            "number" -> Json.toJson("999999"),
            "operationDescription" -> Json.toJson("Test Transaction 1"),
            "operationType" -> Json.toJson("CRE"),
            "operationBalance" -> Json.toJson(10.0),
            "operationDate" -> Json.toJson("10/03/2017")
          ))
        )
      )

      val resultTransaction2 = control.saveTransaction().apply(
        FakeRequest().
          withHeaders(CONTENT_TYPE->"application/json").withJsonBody(
          Json.toJson(Map(
            "number" -> Json.toJson("999999"),
            "operationDescription" -> Json.toJson("Test Transaction 2"),
            "operationType" -> Json.toJson("DEB"),
            "operationBalance" -> Json.toJson(20.0),
            "operationDate" -> Json.toJson("11/03/2017")
          ))
        )
      )

      val resultTransaction3 = control.saveTransaction().apply(
        FakeRequest().
          withHeaders(CONTENT_TYPE->"application/json").withJsonBody(
          Json.toJson(Map(
            "number" -> Json.toJson("999999"),
            "operationDescription" -> Json.toJson("Test Transaction 2"),
            "operationType" -> Json.toJson("CRE"),
            "operationBalance" -> Json.toJson(15.0),
            "operationDate" -> Json.toJson("12/03/2017")
          ))
        )
      )

      val resultDebts = control.getPeriodsOfDebt("999999").apply(
        FakeRequest(GET, "/getPeriodsOfDebt/999999")
      )

      status(resultTransaction1) mustBe OK
      status(resultTransaction2) mustBe OK
      status(resultTransaction3) mustBe OK
      status(resultDebts)        mustBe OK
    }

    "Get error in process of get currents debts" in {

      val control = new HomeController

      val resultTransaction1 = control.saveTransaction.apply(
        FakeRequest().
          withHeaders(CONTENT_TYPE->"application/json").withJsonBody(
          Json.toJson(Map(
            "number" -> Json.toJson("999999"),
            "operationDescription" -> Json.toJson("Test Transaction 1"),
            "operationType" -> Json.toJson("CRE"),
            "operationBalance" -> Json.toJson(10.0),
            "operationDate" -> Json.toJson("10/03/2017")
          ))
        )
      )

      val resultTransaction2 = control.saveTransaction().apply(
        FakeRequest().
          withHeaders(CONTENT_TYPE->"application/json").withJsonBody(
          Json.toJson(Map(
            "number" -> Json.toJson("999999"),
            "operationDescription" -> Json.toJson("Test Transaction 2"),
            "operationType" -> Json.toJson("DEB"),
            "operationBalance" -> Json.toJson(20.0),
            "operationDate" -> Json.toJson("11/03/2017")
          ))
        )
      )

      val resultTransaction3 = control.saveTransaction().apply(
        FakeRequest().
          withHeaders(CONTENT_TYPE->"application/json").withJsonBody(
          Json.toJson(Map(
            "number" -> Json.toJson("999999"),
            "operationDescription" -> Json.toJson("Test Transaction 2"),
            "operationType" -> Json.toJson("CRE"),
            "operationBalance" -> Json.toJson(15.0),
            "operationDate" -> Json.toJson("12/03/2017")
          ))
        )
      )

      val resultDebts = control.getPeriodsOfDebt("99999").apply(
        FakeRequest(GET, "/getPeriodsOfDebt/99999")
      )

      status(resultTransaction1) mustBe OK
      status(resultTransaction2) mustBe OK
      status(resultTransaction3) mustBe OK
      status(resultDebts)        mustBe 500
    }

  }

}
