package jartree

import java.io.{File, FileInputStream}

import jartree.impl.JarCache
import jartree.util.{CaseJarKey, HashJarKeyImpl}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.collection.immutable._
import scala.io.StdIn

/**
  * Created by martonpapp on 27/08/16.
  */
object RunJarCache {

  def main(args: Array[String]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global

    val sourceFile = new File("../jartree/impl/target/product.jar")

    val source = () => new FileInputStream(sourceFile)

    val cacheDir = new File("../jartree/target/jarcache")

    val cache = JarCache(cacheDir)

    val hash = JarCache.calculateHash(source)

    val file = cache.get(HashJarKeyImpl(
      hash.to[Seq]
    ), source)

    Await.result(
      file.map({ file =>
        println(file.getAbsolutePath)
        require(JarCache.calculateHash(() => new FileInputStream(file)).sameElements(hash))
      }),
      Duration.Inf
    )








  }

}
