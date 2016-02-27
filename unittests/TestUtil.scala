package compilerTests

import scala.sys.process._
import java.io._

object TestUtil {
  def runCommands(cmd: Seq[String]): (Int, String, String) = {
    val stdout = new ByteArrayOutputStream
    val stderr = new ByteArrayOutputStream
    val stdoutWriter = new PrintWriter(stdout)
    val stderrWriter = new PrintWriter(stderr)
    val exitValue = cmd.!(ProcessLogger(stdoutWriter.println, stderrWriter.println))
    stdoutWriter.close()
    stderrWriter.close()
    (exitValue, stdout.toString, stderr.toString)
  }

  def checkTestScriptOutput(tagLine: String, testPath: String, bw : BufferedWriter) = {
    bw.write(tagLine + "...\n")
    val (exitStatus, stdOut, stdErr) = TestUtil.runCommands(Seq(testPath))
    bw.write("Exit Status: " + exitStatus + '\n')
    bw.write("Standard Output:\n" + stdOut + '\n')
    bw.write("Standard Error:\n" + stdErr + '\n')
    assert(stdOut.equals(""))
    assert(stdErr.equals(""))
  }
}
