package metaconfig

import scala.util.Try

import cats.data.Xor
import io.circe.Decoder
import io.circe.DecodingFailure
import org.scalatest.FunSuite

class ConfigTest extends FunSuite {
  type Result[T] = Either[Throwable, T]

  @Config
  class Bar(val i: Int, val b: Boolean, val s: String)

  class Foo(val i: Int, val b: Boolean, val s: String) {
    val reader = new Reader[Foo] {
      override def read(any: Any): Result[Foo] = any match {
        case someMap: Map[_, _] =>
          try {
            val map = someMap.asInstanceOf[Map[String, Any]]

            Right(
              new Foo(
                i = implicitly[Reader[Int]]
                  .read(map.getOrElse("i", i))
                  .right
                  .get,
                b = implicitly[Reader[Boolean]]
                  .read(map.getOrElse("b", b))
                  .right
                  .get,
                s = implicitly[Reader[String]]
                  .read(map.getOrElse("s", s))
                  .right
                  .get
              )
            )
          } catch {
            case e: ClassCastException =>
              Left(
                new IllegalArgumentException(
                  s"Expected Map[String, Any], obtained $someMap"))
          }
      }
    }

    override def toString = s"Foo(i=$i, b=$b, s=$s)"
  }

  test("basic") {
    println(new Foo(0, true, "str").reader.read(Map("i" -> 8)))
  }

}
