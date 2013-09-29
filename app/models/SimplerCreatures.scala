package models


class SimpleCreature(val name: String,
                     val weight: Float)


object SimpleCreature {
  import play.api.libs.json._
  import play.api.libs.functional.syntax._
  import play.api.data.validation.ValidationError


 def apply(name: String, weight: Float) = new SimpleCreature(name, weight)
 def unapply(simpleCreature: SimpleCreature):Option[(String,Float)] = Option((simpleCreature.name, simpleCreature.weight))


  def notEqualReads[T](v: T)(implicit r: Reads[T]): Reads[T] = Reads.filterNot(ValidationError("validate.error.unexpected.value", v))( _ == v )

  def skipReads(implicit r: Reads[String]): Reads[String] = r.map( _.substring(2) )

  import play.api.libs.json.Reads._

  val creatureReads: Reads[SimpleCreature] = (
    (__ \ "name").read[String] and
      (__ \ "weight").read[Float]
    )(SimpleCreature.apply _)

  import play.api.libs.json.Writes._

  val creatureWrites: Writes[SimpleCreature] = (
    (__ \ "name").write[String] and
      (__ \ "weight").write[Float]
    )(unlift(SimpleCreature.unapply))

  implicit val creatureFormat: Format[SimpleCreature] = Format(creatureReads, creatureWrites)

}

object SimpleTestCreatures {

  import play.api.libs.json._
  import play.api.libs.functional.syntax._

  def runTest = {
    // Testing Serialization of Creature to Json
    val gizmo = SimpleCreature("gremlins", 1.0F)
    val zombie = SimpleCreature("zombie", 100.0F)

    val zombiejs = Json.obj(
      "name" -> "zombie",
      "weight" -> 100.0
    )

    val json: JsValue = Json.toJson(zombie)
    println(json)
    //assert(json == zombiejs)

    // Testing Deserialization of JSON to Creature (note the dissymetric reading)
    val gizmo2 = SimpleCreature("gremlins", 1.0F)
    val zombie2 = SimpleCreature("zombie", 100.0F)

   // assert(Json.fromJson[SimpleCreature](zombiejs).get == zombie2)
  }

}