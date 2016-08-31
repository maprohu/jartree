package jartree.impl.servlet

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import jartree.impl.{JarCache, JarResolver, JarTree}
import jartree.util.RunRequestImpl
import jartree.{JarTreeRunner, RunRequest}
import sbt.io.IO

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

/**
  * Created by pappmar on 30/08/2016.
  */
class PersistedRunner[Runner](
  workDir: File,
  initializer: JarTree => Unit,
  classLoader: ClassLoader,
  initialRun: RunRequest
)(implicit
  executionContext: ExecutionContext
) extends LazyLogging { self =>
  import PersistedWebappServlet._

  private val jarCache = JarCache(
    new File(workDir, JarCacheDirName)
  )

  private val jarResolver = JarResolver(jarCache)

  val jarTree = JarTree(classLoader, jarResolver)

  def start(runner: Runner => Unit) = {
    try {
      jarTree.clear()

      initializer(jarTree)

      import scala.concurrent.ExecutionContext.Implicits.global

      val result = Await.result(
        jarTree.runInternal[Runner](
          StateIO.readState()
        ).map(
          _.fold(
            Left(_),
            i => Right(runner(i))
          )
        ),
        1.minute
      )

      result.left.foreach({ missing =>
        logger.warn("missing jars: {}", missing)
      })
    } catch {
      case ex : Throwable =>
        logger.error("error starting servlet", ex)
    }

  }

  object StateIO {

    val stateFile =
      new File(workDir, PersistedStateFileName)

    def readState() : RunRequest = synchronized {

      import upickle.default._
      if (stateFile.exists()) {
        try {
          read[RunRequestImpl](IO.read(stateFile))
        } catch {
          case ex : Throwable =>
            logger.error("error reading persisted state", ex)
            initialRun
        }
      } else {
        initialRun
      }

    }

    def writeState(state: RunRequest) : Unit = synchronized {

      workDir.mkdirs()

      val stateImpl = RunRequestImpl(
        state.classLoader(),
        state.className()
      )

      import upickle.default._
      try {
        IO.write(
          stateFile,
          write(stateImpl, indent = 4)
        )
      } catch {
        case ex : Throwable =>
          logger.error("error writing state", ex)
      }
    }

  }


}

object PersistedWebappServlet {

  val PersistedStateFileName = "state.dat"
  val JarCacheDirName = "jar-cache"

}
