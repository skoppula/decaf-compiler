import org.scalatest.FunSuite

import scala.sys.process._

class ScannerTestSuite extends FunSuite {
  test("Scanner passes the 6.035 custom scanner tests") {
    val result = "./tests/scanner/custom/test.sh" !!;
    println(result)
    assert(result.equals(""))
  }

	test("Scanner passes the 6.035 public scanner tests") {
		val result = "./tests/scanner/test.sh" !!;
    println(result)
    assert(result.equals(""))
	}
  test("Scanner passes the 6.035 hidden scanner tests") {
    val result = "./tests/scanner-hidden/test.sh" !!;
    println(result)
    assert(result.equals(""))
  }
}