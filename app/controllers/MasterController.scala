package controllers

import play.api.mvc.{BaseController, ControllerComponents}

import javax.inject.Inject

class MasterController @Inject()(val controllerComponents: ControllerComponents) extends BaseController{
  //All remaining controllers can take from this
  //But this inherritance is pretty weird

}
