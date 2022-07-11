package controllers

import models.{ControllerAListItem, NewControllerAListItem}
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}

import javax.inject.{Inject, Singleton}
import scala.collection.mutable
import play.api.libs.json._

@Singleton
class ControllerA @Inject()(val controllerComponents: ControllerComponents) extends BaseController{

  private val ItemsList = new mutable.ListBuffer[ControllerAListItem]()
  ItemsList += ControllerAListItem(1,"FirstItemControllerA")
  ItemsList += ControllerAListItem(2,"SecondItemControllerA")
  ItemsList += ControllerAListItem(3,"ThirdItemControllerA")
  ItemsList += ControllerAListItem(4,"FourthItemControllerA")
  ItemsList += ControllerAListItem(5,"FifthItemControllerA")

  implicit val itemsList = Json.format[ControllerAListItem]
  implicit val newItem = Json.format[NewControllerAListItem]

  def getAll: Action[AnyContent] = Action {
    if(ItemsList.isEmpty) {
      NoContent
    } else {
      Ok(Json.toJson(ItemsList))
    }
  }

  def getById(itemId: Long): Action[AnyContent] = Action {
    val item = ItemsList.find(_.id == itemId)
    item match {
      case Some(item) => Ok(Json.toJson(item))
      case None => NotFound
    }
  }

  def updateItem(itemId: Long): Action[JsValue] = Action(parse.json) { implicit request =>
    val item = ItemsList.find(_.id == itemId)
    val content: JsValue = request.body
    item match {
      case Some(item) =>
        val value = (content \ "value").as[String]
        val newItem = item.copy(id = itemId, value = value)
        ItemsList.update(ItemsList.indexOf(item), newItem)
        Accepted(Json.toJson(newItem))
      case None => NotFound
    }
  }

  def deleteItem(itemId: Long): Action[AnyContent] = Action {
    val item = ItemsList.find(_.id == itemId)
    item match {
      case Some(item) =>
        ItemsList -= item
        Accepted
      case None => NotFound
    }
  }

  def addItem: Action[AnyContent] = Action { implicit request =>

    val content = request.body
    val jsonContent = content.asJson
    val addingItem: Option[NewControllerAListItem] = jsonContent.flatMap(Json.fromJson[NewControllerAListItem](_).asOpt)

    addingItem match {
      case Some(newItem) =>
        val next_index = ItemsList.map(_.id).max + 1
        val toBeAdded = ControllerAListItem(next_index, newItem.value)
        ItemsList += toBeAdded
        Created(Json.toJson(toBeAdded))
      case None => BadRequest
    }
  }
}
