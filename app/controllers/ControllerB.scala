package controllers

import models.{ControllerBListItem, NewControllerAListItem, NewControllerBListItem}
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}

import javax.inject.Inject
import scala.collection.mutable

class ControllerB @Inject()(val controllerComponents: ControllerComponents) extends BaseController{

  private val ItemsList = new mutable.ListBuffer[ControllerBListItem]()
  ItemsList += ControllerBListItem(1,"FirstItemControllerB")
  ItemsList += ControllerBListItem(2,"SecondItemControllerB")
  ItemsList += ControllerBListItem(3,"ThirdItemControllerB")
  ItemsList += ControllerBListItem(4,"FourthItemControllerB")
  ItemsList += ControllerBListItem(5,"FifthItemControllerB")

  implicit val itemsList = Json.format[ControllerBListItem]
  implicit val newItem = Json.format[NewControllerBListItem]

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
    val addingItem: Option[NewControllerBListItem] = jsonContent.flatMap(Json.fromJson[NewControllerBListItem](_).asOpt)

    addingItem match {
      case Some(newItem) =>
        val next_index = ItemsList.map(_.id).max + 1
        val toBeAdded = ControllerBListItem(next_index, newItem.value)
        ItemsList += toBeAdded
        Created(Json.toJson(toBeAdded))
      case None => BadRequest
    }
  }
}