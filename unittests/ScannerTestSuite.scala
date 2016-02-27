package compilerTests

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfterAll
import java.io._

class ScannerTestSuite extends FunSuite with BeforeAndAfterAll {

  val file = new File("tests/test_output/scanner_test.out")
  val bw = new BufferedWriter(new FileWriter(file))

	test("Scanner passes the 6.035 public scanner tests") {
    TestUtil.checkTestScriptOutput("Running the 6.035 public scanner tests", "./tests/scanner/test.sh", bw)
	}

  test("Scanner passes the 6.035 hidden scanner tests") {
    TestUtil.checkTestScriptOutput("Running the 6.035 hidden scanner tests", "./tests/scanner-hidden/test.sh", bw)
  }

  test("Scanner passes the 6.035 custom scanner tests") {
    TestUtil.checkTestScriptOutput("Running the 6.035 custom scanner tests", "./tests/scanner/custom/test.sh", bw)
  }

  override def afterAll {
    bw.close()
  }
}