package metaconfig

import cats.data.Xor
import io.circe.Decoder
import io.circe.DecodingFailure
import org.scalatest.FunSuite

class ConfigTest extends FunSuite {

  @Config
  class Foo(i: Int, b: Boolean, s: String)

  case class Bar(i: Int, b: Boolean, s: String) {
    val decoder: Decoder[Bar] = Decoder.instance { c =>
      val invalidFields = c.fields.getOrElse(Nil).filter { x =>
        !Set("i", "b", "s").contains(x)
      }
      if (invalidFields.nonEmpty) {
        Xor.Left(DecodingFailure(s"Unknown fields: $invalidFields", c.history))
      } else {
        for {
          i_ <- c.get[Int]("i").recover {
            case DecodingFailure(msg, _) if msg.startsWith("Attempt") =>
              println(msg)
              i
          }
          b_ <- c.get[Boolean]("b").orElse(Xor.Right(b))
          s_ <- c.get[String]("s").orElse(Xor.Right(s))
        } yield Bar(i_, b_, s_)
      }
    }
  }

  test("basic") {
    new Foo(1, true, "str")
    import io.circe.parser._
    parse("""
            |{
            |  "b": false,
            |  "i": "string"
            |}
            |""".stripMargin).map { json =>
      val decoded = Bar(1, b = true, "string").decoder.decodeJson(json)
      println(decoded)
    }
  }

}
