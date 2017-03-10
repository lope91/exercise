package controllers

import java.util
import javax.inject._

import play.api._
import play.api.data.Form
import play.api.mvc._
import models.Account

import scala.collection.immutable.HashMap
import scala.concurrent.Future

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject() extends Controller {

  var accounts = new util.ArrayList[Account]

  def saveInformation =  Action { request =>
    val account = request.body.asJson.get.as[Account]
    accounts.add(account)

    Ok
  }

}

