package com.github.tminglei.slickpg

import java.util.concurrent.Executors

import org.json4s._
import org.scalatest.funsuite.AnyFunSuite
import slick.jdbc.{GetResult, PostgresProfile}

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

class PgJson4sSupportSuite extends AnyFunSuite with PostgresContainer {
  implicit val testExecContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

  trait MyPostgresProfile extends PostgresProfile
                            with PgJson4sSupport
                            with array.PgArrayJdbcTypes {
    /// for json support
    override val pgjson = "jsonb"
    type DOCType = org.json4s.native.Document
    override val jsonMethods = org.json4s.native.JsonMethods.asInstanceOf[JsonMethods[DOCType]]

    override val api: API = new API {}

    val plainAPI = new API with Json4sJsonPlainImplicits

    ///
    trait API extends JdbcAPI with JsonImplicits {
      implicit val strListTypeMapper = new SimpleArrayJdbcType[String]("text").to(_.toList)
      implicit val json4sJsonArrayTypeMapper =
        new AdvancedArrayJdbcType[JValue](pgjson,
          (s) => utils.SimpleArrayUtils.fromString[JValue](jsonMethods.parse(_))(s).orNull,
          (v) => utils.SimpleArrayUtils.mkString[JValue](j=>jsonMethods.compact(jsonMethods.render(j)))(v)
        ).to(_.toList)
    }
  }
  object MyPostgresProfile extends MyPostgresProfile

  ///
  import MyPostgresProfile.api._
  import MyPostgresProfile.jsonMethods._

  lazy val db = Database.forURL(url = container.jdbcUrl, driver = "org.postgresql.Driver")

  case class JsonBean(id: Long, json: JValue, jsons: List[JValue])

  class JsonTestTable(tag: Tag) extends Table[JsonBean](tag, "JsonTest1") {
    def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
    def json = column[JValue]("json")
    def jsons = column[List[JValue]]("jsons")

    def * = (id, json, jsons) <> (JsonBean.tupled, JsonBean.unapply)
  }
  val JsonTests = TableQuery[JsonTestTable]

  //------------------------------------------------------------------------------

  val testRec1 = JsonBean(33L, parse(""" { "a":101, "b":"aaa", "c":[3,4,5,9] } """), List(parse(""" { "a":101, "b":"aaa", "c":[3,4,5,9] } """), parse(""" { "a":102, "b":["bbb"], "c":[3,5,7,9] } """)))
  val testRec2 = JsonBean(35L, parse(""" [ {"a":"v\\n1\\u0000","b":2}, {"a":"v5","b":3} ] """), List(parse(""" [ {"a":"v1","b":2}, {"a":"v5","b":3} ] """)))
  val testRec3 = JsonBean(37L, parse(""" ["a", "b"] """), List(parse("{\"level\":\"DAILY_LEVEL\",\"start\":{\"year\":2013,\"month\":1,\"day\":1},\"end\":{\"year\":2016,\"month\":1,\"day\":1}}")))

  test("Json4s Lifted support") {
    val json1 = parse(""" {"a":"v\\n1\\u0000","b":2} """)
    val json2 = parse(""" {"a":"v5","b":3} """)

    Await.result(db.run(
      DBIO.seq(
        JsonTests.schema create,
        ///
        JsonTests forceInsertAll List(testRec1, testRec2, testRec3)
      ).andThen(
        DBIO.seq(
          JsonTests.filter(_.id === testRec2.id.bind).map(_.json).result.head.map(
            r => assert(JArray(List(json1,json2)) === r)
          ),
          JsonTests.sortBy(_.json.+>>("a")).to[List].result.map(
            r => assert(List(testRec1, testRec2, testRec3) === r)
          ),
          // ->>/->
          JsonTests.filter(_.json.+>>("a") === "101").map(_.json.+>>("c")).result.head.map(
            r => assert("[3,4,5,9]" === r.replace(" ", ""))
          ),
          JsonTests.filter(_.json.+>>("a") === "101".bind).map(_.json.+>("c")).result.head.map(
            r => assert(JArray(List(JInt(3), JInt(4), JInt(5), JInt(9))) === r)
          ),
          JsonTests.filter(_.id === testRec2.id).map(_.json.~>(1)).result.head.map(
            r => assert(json2 === r)
          ),
          JsonTests.filter(_.id === testRec2.id).map(_.json.~>>(1)).result.head.map(
            r => assert("""{"a":"v5","b":3}""" === r.replace(" ", ""))
          ),
          // #>>/#>
          JsonTests.filter(_.id === testRec1.id).map(_.json.#>(List("c"))).result.head.map(
            r => assert(parse("[3,4,5,9]") === r)
          ),
          JsonTests.filter(_.json.#>>(List("a")) === "101").result.head.map(
            r => assert(testRec1 === r)
          ),
          // {}_array_length
          JsonTests.filter(_.id === testRec2.id).map(_.json.arrayLength).result.head.map(
            r => assert(2 === r)
          ),
          // {}_array_elements
          JsonTests.filter(_.id === testRec2.id).map(_.json.arrayElements).to[List].result.map(
            r => assert(List(json1, json2) === r)
          ),
          JsonTests.filter(_.id === testRec2.id).map(_.json.arrayElements).result.head.map(
            r => assert(json1 === r)
          ),
          // {}_array_elements_text
          JsonTests.filter(_.id === testRec2.id).map(_.json.arrayElementsText).result.head.map(
            r => assert(compact(render(json1)).replace(" ", "") === r.replace(" ", ""))
          ),
          // {}_object_keys
          JsonTests.filter(_.id === testRec1.id).map(_.json.objectKeys).to[List].result.map(
            r => assert(List("a","b","c") === r)
          ),
          JsonTests.filter(_.id === testRec1.id).map(_.json.objectKeys).result.head.map(
            r => assert("a" === r)
          ),
          // @>
          JsonTests.filter(_.json @> parse(""" {"b":"aaa"} """)).result.head.map(
            r => assert(33L === r.id)
          ),
          JsonTests.filter(_.json @> parse(""" [{"a":"v5"}] """)).result.head.map(
            r => assert(35L === r.id)
          ),
          // <@
          JsonTests.filter(parse(""" {"b":"aaa"} """) <@: _.json).result.head.map(
            r => assert(33L === r.id)
          ),
          // {}_typeof
          JsonTests.filter(_.id === testRec1.id).map(_.json.+>("a").jsonType).result.head.map(
            r => assert("number" === r.toLowerCase)
          ),
          // ?
          JsonTests.filter(_.json ?? "b".bind).to[List].result.map(
            r => assert(List(testRec1, testRec3) === r)
          ),
          // ?|
          JsonTests.filter(_.json ?| List("a", "c").bind).to[List].result.map(
            r => assert(List(testRec1, testRec3) === r)
          ),
          // ?&
          JsonTests.filter(_.json ?& List("a", "c").bind).to[List].result.map(
            r => assert(List(testRec1) === r)
          ),
          // ||
          JsonTests.filter(_.id === 33L).map(_.json || parse(""" {"d":"test"} """)).result.head.map(
            r => assert(""" {"a": 101, "b": "aaa", "c": [3, 4, 5, 9], "d": "test"} """.replace(" ", "") === compact(render(r)))
          ),
          // -
          JsonTests.filter(_.id === 33L).map(_.json - "c".bind).result.head.map(
            r => assert(""" {"a": 101, "b": "aaa"} """.replace(" ", "") === compact(render(r)))
          ),
          // #-
          JsonTests.filter(_.id === 33L).map(_.json #- List("c")).result.head.map(
            r => assert(""" {"a": 101, "b": "aaa"} """.replace(" ", "") === compact(render(r)))
          ),
          // #-
          JsonTests.filter(_.id === 33L).map(_.json.set(List("c"), parse(""" [1] """).bind)).result.head.map(
            r => assert(""" {"a": 101, "b": "aaa", "c": [1]} """.replace(" ", "") === compact(render(r)))
          )
        )
      ).andFinally(
        JsonTests.schema drop
      ).transactionally
    ), Duration.Inf)
  }

  //------------------------------------------------------------------------------

  case class JsonBean1(id: Long, json: JValue)

  test("Json4s Plain SQL support") {
    import MyPostgresProfile.plainAPI._

    implicit val getJsonBeanResult = GetResult(r => JsonBean1(r.nextLong(), r.nextJson()))

    val b = JsonBean1(34L, parse(""" { "a":101, "b":"aaa", "c":[3,4,5,9] } """))

    Await.result(db.run(
      DBIO.seq(
        sqlu"""create table JsonTest1(
              id int8 not null primary key,
              json #${MyPostgresProfile.pgjson} not null)
          """,
        ///
        sqlu""" insert into JsonTest1 values(${b.id}, ${b.json}) """,
        sql""" select * from JsonTest1 where id = ${b.id} """.as[JsonBean1].head.map(
          r => assert(b === r)
        ),
        ///
        sqlu"drop table if exists JsonTest1 cascade"
      ).transactionally
    ), Duration.Inf)
  }
}
