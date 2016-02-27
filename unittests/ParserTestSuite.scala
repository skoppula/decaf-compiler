import org.scalatest.FunSuite

import scala.sys.process._

class ParserTestSuite extends FunSuite {
  test("Parser passes the 6.035 public parser tests") {
    val result = "./tests/parser/test.sh" !!;
    println(result);
    assert(result.equals(""))
  }

  test("Parser passes the 6.035 hidden parser tests") {
    val result = "./tests/parser-hidden/test.sh" !!;
    println(result);
    assert(result.equals(""))
  }

  test("Parser passes the 6.035 custom tests") {
    val result = "./tests/parser/custom/test.sh" !!;
    println(result);
    assert(result.equals(""))
  }
}