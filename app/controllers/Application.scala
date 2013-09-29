package controllers

import play.api._
import play.api.mvc._
import models.{SimpleTestCreatures, TestCreatures}

object Application extends Controller {

  def index = Action {

    TestCreatures.runTest


    SimpleTestCreatures.runTest

    Ok(views.html.index("Your new application is ready."))
  }

}