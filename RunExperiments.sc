#!/usr/bin/env -S scala-cli shebang --scala-version 3.8.1

import java.io.File
import java.nio.file.Path
import java.nio.file.{ Files, Paths, StandardCopyOption }
import java.util.Comparator.reverseOrder
import scala.sys.process.*
import scala.jdk.CollectionConverters.*
import scala.concurrent.duration.*
import scala.concurrent.{ Future, Await, ExecutionContext, TimeoutException }
import ExecutionContext.Implicits.global

object ExperimentRunner:

  private val basePackage = "it.unibo.pslab"
  private val evaluationDirectory = Paths.get("evaluation")
  private val maxWorkers = 32

  def main(args: Array[String]): Unit =
    try
      prepare()
      runExperimentsUsingBroadcastComm(maxWorkers)
      runExperimentsUsingSelectiveComm(maxWorkers)
      plot()
      info(s"All done! Generated plots are available at ${evaluationDirectory.toAbsolutePath()}")
    catch case exception: Exception => error(exception.getMessage())

  def prepare(): Unit =
    mkdir(evaluationDirectory)
    cleanup(evaluationDirectory)

  def runExperimentsUsingSelectiveComm(maxWorkers: Int): Unit =
    info("Running MatMul Master-Worker program with selective style communications")
    runExperiments(
      maxWorkers,
      fqClass = s"$basePackage.matmul.MatMulMaster",
      label = "selective experiment",
    )

  def runExperimentsUsingBroadcastComm(maxWorkers: Int): Unit =
    info("Running MatMul Master-Worker program with broadcasting style communications")
    runExperiments(
      maxWorkers,
      fqClass = s"$basePackage.matmul.InefficientMatMulMaster",
      label = "broadcasting (inefficient) experiment",
    )

  def runExperiments(maxWorkers: Int, fqClass: String, label: String): Unit =
    var workers = 1
    while workers <= maxWorkers do
      info(s"Starting $label with $workers worker(s)")
      val exitCode = runScalaMain(fqClass, workers.toString, s"$workers-workers").!
      if exitCode != 0 then
        removeMatching(evaluationDirectory, s".*${workers}(-workers)?\\.csv")
        throw RuntimeException(
          s"Experiment with $workers worker(s) failed. Stopping further experiments for this configuration.",
        )
      else workers *= 2
      Thread.sleep(1_000)

  def plot(): Unit =
    info("Plotting results")
    ensurePythonEnvironment()
    val exitCode = Process(Seq(pythonExecutable, "plot_results.py")).!
    if exitCode != 0 then throw RuntimeException(s"Failed to plot results.")

  def ensurePythonEnvironment(): Unit =
    if !venvExists then
      createVenv()
      installPythonDependencies()

  def venvExists: Boolean =
    Files.exists(Paths.get(if isWindows then "venv\\Scripts\\python.exe" else "venv/bin/python"))

  def createVenv(): Unit =
    val exitCode = Process(Seq("python3", "-m", "venv", "venv")).!
    if exitCode != 0 then throw RuntimeException(s"Failed to create virtual environment.")

  def installPythonDependencies(): Unit =
    val pip = if isWindows then "venv\\Scripts\\pip.exe" else "venv/bin/pip"
    val exitCode = Process(Seq(pip, "install", "-r", "requirements.txt")).!
    if exitCode != 0 then throw RuntimeException(s"Failed to install Python dependencies.")

  def pythonExecutable: String = if isWindows then "venv\\Scripts\\python.exe" else "venv/bin/python"

  def isWindows: Boolean = System.getProperty("os.name").toLowerCase.contains("win")

  def cleanup(path: Path = currentPath): Unit =
    Files.walk(path).sorted(reverseOrder()).filter(_ != path).forEach(Files.delete)

  def mkdir(path: Path): Unit = if !Files.exists(path) then Files.createDirectories(path)

  def runScalaMain(fqn: String, args: String*): ProcessBuilder =
    Process(Seq("./mill", "examples.runMain", fqn) ++ args)

  def removeMatching(directory: Path, pattern: String): Unit =
    Files.list(directory).filter(p => p.getFileName.toString.matches(pattern)).forEach(Files.delete)

  def currentPath: Path = Paths.get(".").toAbsolutePath.normalize()

  def info(message: String): Unit = println("=" * 120 + s"\n$message\n" + "=" * 120)

  def error(message: String): Unit = System.err.println(s"\n\u001b[31mERROR: $message\u001b[0m\n")
end ExperimentRunner

ExperimentRunner.main(args)
