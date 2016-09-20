package metaconfig

import cats.data.Xor
import io.circe.Decoder
import io.circe.DecodingFailure
import org.scalatest.FunSuite

class ConfigTest extends FunSuite {
  type Result[T] = Either[Throwable, T]

  trait Reader[T] {
    def read(any: Any): Result[T]
  }
  object Reader {
    def instance[T](f: PartialFunction[Any, Result[T]]) =
      new Reader[T] {
        override def read(any: Any) = f(any)
      }
    implicit val intR = instance[Int] { case x: Int => Right(x) }
    implicit val stringR = instance[String] { case x: String => Right(x) }
    implicit val boolR = instance[scala.Boolean] {
      case x: Boolean => Right(x)
    }
  }

  class Foo(val i: Int, val b: Boolean, val s: String) {
    def merge(map: Map[String, Any]) = {
      // validate fields
      new Foo(
        i = implicitly[Reader[Int]].read(map.getOrElse("i", i)).right.get,
        b = implicitly[Reader[Boolean]].read(map.getOrElse("b", b)).right.get,
        s = implicitly[Reader[String]].read(map.getOrElse("s", s)).right.get
      )
    }

    override def toString = s"Foo(i=$i, b=$b, s=$s)"
  }

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
      println(new Foo(0, true, "str").merge(Map("i" -> 8)))
    }
  }

}
