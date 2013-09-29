package models

class Creature(val name: String,
               val isDead: Boolean,
               val weight: Float,
               val email: String, // email format and minLength(5)
               //favorites: (String, Int), // the stupid favorites
               val friends: List[Creature] = Nil, // yes by default it has no friend
               val social: Option[String] = None // by default, it's not social
                )


object Creature {
  // import just Reads helpers in scope

  import play.api.libs.json._
  import play.api.libs.functional.syntax._
  import play.api.data.validation.ValidationError

  // defines a custom reads to be reused
  // a reads that verifies your value is not equal to a give value
  def notEqualReads[T](v: T)(implicit r: Reads[T]): Reads[T] = Reads.filterNot(ValidationError("validate.error.unexpected.value", v))(_ == v)

  def skipReads(implicit r: Reads[String]): Reads[String] = r.map(_.substring(2))

  def apply(name: String, isDead: Boolean, weight: Float,
            email: String, friends: List[Creature] = Nil,
            social: Option[String] = None) = new Creature(name, isDead, weight, email, friends, social)

  def unapply(creature: Creature): Option[(String, Boolean, Float, String, List[Creature], Option[String])] =
    Option((creature.name,creature.isDead, creature.weight, creature.email, creature.friends, creature.social))

  import play.api.libs.json.Reads._

  val creatureReads: Reads[Creature] = (
    (__ \ "name").read[String] and
      (__ \ "isDead").read[Boolean] and
      (__ \ "weight").read[Float] and
      (__ \ "email").read(email keepAnd minLength[String](5)) and
      (__ \ "friends").lazyRead(list[Creature](creatureReads)) and
      (__ \ "social").readNullable[String]
    )(Creature.apply _)

  import play.api.libs.json.Writes._

  val creatureWrites: Writes[Creature] = (
    (__ \ "name").write[String] and
      (__ \ "isDead").write[Boolean] and
      (__ \ "weight").write[Float] and
      (__ \ "email").write[String] and
      (__ \ "friends").lazyWrite(Writes.traversableWrites[Creature](creatureWrites)) and
      (__ \ "social").write[Option[String]]
    )(unlift(Creature.unapply))

  implicit val creatureFormat: Format[Creature] = Format(creatureReads, creatureWrites)

}

object TestCreatures {

  import play.api.libs.json._

  def runTest = {
    // Testing Serialization of Creature to Json
    val gizmo = Creature("gremlins", false, 1.0F, "gizmo@midnight.com", List(), Some("@gizmo"))
    val zombie = Creature("zombie", true, 100.0F, "shaun@dead.com", List(gizmo), None)

    val zombiejs = Json.obj(
      "name" -> "zombie",
      "isDead" -> true,
      "weight" -> 100.0,
      "email" -> "shaun@dead.com",
      "friends" -> Json.arr(
        Json.obj(
          "name" -> "gremlins",
          "isDead" -> false,
          "weight" -> 1.0,
          "email" -> "gizmo@midnight.com",
          "friends" -> Json.arr(),
          "social" -> "@gizmo"
        )
      ),
      "social" -> JsNull
    )

    val json: JsValue = Json.toJson(zombie)
    println(json)
    assert(json == zombiejs)

    // Testing Deserialization of JSON to Creature (note the dissymetric reading)
    val gizmo2 = Creature("gremlins", false, 1.0F, "gizmo@midnight.com", List(), Some("@gizmo"))
    val zombie2 = Creature("zombie", true, 100.0F, "shaun@dead.com", List(gizmo2), None)

    val creature: Creature = Json.fromJson[Creature](zombiejs).get
    println(s"creature: ${creature.name} ${creature.email}")

    //assert(creature == zombie2)
  }

}