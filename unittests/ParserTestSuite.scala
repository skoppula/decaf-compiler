package compilerTests

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfterAll
import java.io._

class ParserTestSuite extends FunSuite with BeforeAndAfterAll {

  val file = new File("tests/test_output/parser_test.out")
  val bw = new BufferedWriter(new FileWriter(file))

  test("Parser passes the 6.035 public parser tests") {
    TestUtil.checkTestScriptOutput("Running the 6.035 public parser tests", "./tests/parser/test.sh", bw)
  }

  test("Parser passes the 6.035 hidden parser tests") {
    TestUtil.checkTestScriptOutput("Running the 6.035 hidden parser tests", "./tests/parser-hidden/test.sh", bw)
  }

  test("Parser passes the 6.035 custom parser tests") {
    TestUtil.checkTestScriptOutput("Running the 6.035 custom parser tests", "./tests/parser/custom/test.sh", bw)
  }
  override def afterAll {
    bw.close()
  }
}