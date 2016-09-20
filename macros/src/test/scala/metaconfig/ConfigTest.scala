package metaconfig

import org.scalatest.FunSuite

class ConfigTest extends FunSuite {

  @Config
  class Foo(i: Int, b: Boolean, s: String) {
    2
    3
  }

  test("basic") {
    new Foo(1, true, "str")
  }

}
