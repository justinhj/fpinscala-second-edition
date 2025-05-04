package fpinscala.exercises.parallelism

import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Gen.`**`
import fpinscala.answers.testing.exhaustive.Prop.*
import fpinscala.exercises.common.Common.*
import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.parallelism.Nonblocking.*
import scala.compiletime.uninitialized
import java.util.concurrent.*

class NonblockingSuite extends PropSuite:

  private var es: ExecutorService = uninitialized
  private var scheduler: ScheduledExecutorService = uninitialized

  override def beforeEach(c: BeforeEach): Unit = {
    println(s"Initializing ExecutorService ${c.test.name}")
    es = Executors.newFixedThreadPool(5)
    scheduler = Executors.newScheduledThreadPool(1)
    startStatusPrinter(es, scheduler)
  }

  override def afterEach(c: AfterEach): Unit = {
    println("Initiating ExecutorService shutdown")
    es.shutdown()
    val terminated = es.awaitTermination(3, TimeUnit.SECONDS)
    if (terminated) {
      println("ExecutorService terminated successfully")
    } else {
      println("ExecutorService did not terminate within 3 seconds")
      val threadMXBean = java.lang.management.ManagementFactory.getThreadMXBean
      val threadInfo = threadMXBean.dumpAllThreads(true, true)
      println("Active threads:")
      threadInfo.foreach { info =>
        println(s"Thread: ${info.getThreadName}, State: ${info.getThreadState}")
        info.getStackTrace.foreach { element =>
          println(s"  at $element")
        }
        println()
      }
    }
    println("Shutting down scheduler")
    scheduler.shutdown()
    scheduler.awaitTermination(1, TimeUnit.SECONDS)
  }

  // Function to print ExecutorService status every 3 seconds
  def startStatusPrinter(es: ExecutorService, scheduler: ScheduledExecutorService): Unit = {
    val statusTask = new Runnable {
      def run(): Unit = {
        if (!es.isShutdown) {
          try {
            val pool = es.asInstanceOf[java.util.concurrent.ThreadPoolExecutor]
            println(
              s"ExecutorService Status: " +
              s"Active Tasks: ${pool.getActiveCount}, " +
              s"Queued Tasks: ${pool.getQueue.size}, " +
              s"Completed Tasks: ${pool.getCompletedTaskCount}, " +
              s"Pool Size: ${pool.getPoolSize}, " +
              s"Shutdown: ${pool.isShutdown}, " +
              s"Terminating: ${pool.isTerminating}, " +
              s"Terminated: ${pool.isTerminated}"
            )
          } catch {
            case e: Exception =>
              println(s"Error retrieving ExecutorService status: $e")
          }
        } else {
          println("ExecutorService is shut down; stopping status printer")
          scheduler.shutdown()
        }
      }
    }
    scheduler.scheduleAtFixedRate(statusTask, 0, 3, TimeUnit.SECONDS)
  }

  private val genParBoolean: Gen[Par[Boolean]] = Gen.boolean.map(Par.unit)
  private val genParInt: Gen[Par[Int]] = genShortNumber.map(Par.unit)
  private val genParString: Gen[Par[String]] = genString.map(Par.unit)
  private val genParParString: Gen[Par[Par[String]]] = genString.map(str => Par.unit(Par.unit(str)))
  private val genListOfParString: Gen[List[Par[String]]] =
    genNonEmptyList[Par[String]](genParString)
  private val genMap: Gen[Map[Int, Par[String]]] =
    Gen.listOfN(20, genParString).map(list => list.toIndexedSeq.indices.map(i => i -> list(i)).toMap)

  test("Nonblocking.choice")(genParInt ** genParInt ** genParBoolean):
    case t ** f ** p =>
      checkChoice(t, f, p)(Par.choice[Int](p)(t, f))

  test("Nonblocking.choiceN")(genParInt ** genListOfParString):
    case p ** ps =>
      checkChoiceN(p, ps)(Par.choiceN[String](p)(ps))

  test("Nonblocking.choiceViaChoiceN")(genParInt ** genParInt ** genParBoolean):
    case t ** f ** p =>
      checkChoice(t, f, p)(Par.choiceViaChoiceN[Int](p)(t, f))

  test("Nonblocking.choiceMap")(genParInt ** genMap):
    case p ** ps =>
      val pc = Par.choiceMap[Int, String](p)(ps)
      val actual: String = pc.run(es)
      val key = p.run(es)
      val expected: String = ps(key).run(es)
      assertEquals(actual, expected)

  test("Nonblocking.chooser")(genParInt ** genMap):
    case p ** ps =>
      checkFlatMap(p, ps)(Par.chooser[Int, String](p)(ps))

  test("Nonblocking.choiceViaFlatMap")(genParInt ** genParInt ** genParBoolean):
    case t ** f ** p =>
      checkChoice(t, f, p)(Par.choiceViaFlatMap[Int](p)(f, t))

  test("Nonblocking.choiceNViaFlatMap")(genParInt ** genListOfParString):
    case p ** ps =>
      checkChoiceN(p, ps)(Par.choiceNViaFlatMap[String](p)(ps))

  test("Nonblocking.join")(genParParString): p =>
    val pc = Par.join[String](p)
    assertEquals(pc.run(es), p.run(es).run(es))

  test("Nonblocking.joinViaFlatMap")(genParParString): p =>
    val pc = Par.joinViaFlatMap[String](p)
    assertEquals(pc.run(es), p.run(es).run(es))

  test("Nonblocking.flatMapViaJoin")(genParInt ** genMap):
    case p ** ps =>
      checkFlatMap(p, ps)(Par.flatMapViaJoin[Int, String](p)(ps))

  private def checkChoice(t: Par[Int], f: Par[Int], p: Par[Boolean])(choice: Par[Int]) =
    val actual = choice.run(es)
    val expected = if p.run(es) then t.run(es) else f.run(es)
    assertEquals(actual, expected)

  private def checkChoiceN(p: Par[Int], ps: List[Par[String]])(choice: Par[String]) =
    val actual = choice.run(es)
    val i = p.run(es) % ps.length
    val expected = ps(i).run(es)
    assertEquals(actual, expected)

  private def checkFlatMap(p: Par[Int], ps: Map[Int, Par[String]])(choice: Par[String]) =
    val actual: String = choice.run(es)
    val key = p.run(es)
    val expected: String = ps(key).run(es)
    assertEquals(actual, expected)
