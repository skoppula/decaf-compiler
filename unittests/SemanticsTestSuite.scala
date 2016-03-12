package compilerTests

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfterAll
import java.io._

class SemanticsTestSuite extends FunSuite with BeforeAndAfterAll {

  val file = new File("tests/test_output/semantics_test.out")
  val bw = new BufferedWriter(new FileWriter(file))

  test("IR passes the 6.035 public semantics tests") {
    TestUtil.checkTestScriptOutput("Running the 6.035 public semantics tests", "./tests/semantics/test.sh", bw)
  }

  test("IR passes the 6.035 hidden semantics tests") {
    TestUtil.checkTestScriptOutput("Running the 6.035 hidden semantics tests", "./tests/semantics-hidden/test.sh", bw)
  }

  override def afterAll {
    bw.close()
  }
}