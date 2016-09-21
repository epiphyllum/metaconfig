package metaconfig

import org.scalatest.FunSuite

class ConfigTest extends FunSuite {
  type Result[T] = Either[Throwable, T]

  @Config
  case class Inner(nest: Int)

  @Config
  case class Outer(i: Int, inner: Inner) {
    implicit val innerReader: Reader[Inner] = inner.reader
  }

  @Config
  case class Bar(i: Int, b: Boolean, s: String)

  @Config
  case class HasList(i: Seq[Int])

  val b = new Bar(0, true, "str")
  test("invalid field") {
    assert(
      b.reader.read(Map("is" -> 2, "var" -> 3)) ==
        Left(ConfigError("Invalid fields: is, var")))
  }

  test("read OK") {
    assert(b.reader.read(Map("i" -> 2)) == Right(b.copy(i = 2)))
    assert(b.reader.read(Map("s" -> "str")) == Right(b.copy(s = "str")))
    assert(b.reader.read(Map("b" -> true)) == Right(b.copy(b = true)))
    assert(
      b.reader.read(
        Map(
          "i" -> 3,
          "b" -> true,
          "s" -> "rand"
        )) == Right(b.copy(i = 3, s = "rand", b = true)))
  }
  test("unexpected type") {
    val msg =
      "Error reading field i on class Bar. Expected argument of type Integer. Obtained value 'str' of type String."
    assert(b.reader.read(Map("i" -> "str")) == Left(ConfigError(msg)))
  }

  test("write OK") {
    assert(
      b.fields == Map(
        "i" -> 0,
        "b" -> true,
        "s" -> "str"
      ))
  }
  test("nested OK") {
    val m: Map[String, Any] = Map(
      "i" -> 4,
      "inner" -> Map(
        "nest" -> 5
      )
    )
    val o = Outer(2, Inner(3)).reader.read(m)
    println(o)
  }

}
