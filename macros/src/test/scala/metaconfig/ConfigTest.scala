package metaconfig

import org.scalatest.FunSuite

class ConfigTest extends FunSuite {
  type Result[T] = Either[Throwable, T]

  @Config
  case class Bar(i: Int, b: Boolean, s: String)

  class Foo(val i: Int, val b: Boolean, val s: String) {
    val reader = new Reader[Foo] {
      override def read(any: Any): Result[Foo] = any match {
        case String2AnyMap(someMap) =>
          val map = someMap.asInstanceOf[Map[String, Any]]
          Right(
            new Foo(
              i = implicitly[Reader[Int]].read(map.getOrElse("i", i)).right.get,
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
      }
    }

    override def toString = s"Foo(i=$i, b=$b, s=$s)"
  }

  test("basic") {
    val b = new Bar(0, true, "str")
    val read = b.reader.read(Map("is" -> 2))
    val write = b.fields
    println(read)
    println(write)
  }

}
