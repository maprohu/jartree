package jartree

import java.io.{File, FileInputStream}

import scala.io.StdIn

/**
  * Created by martonpapp on 27/08/16.
  */
object RunJarCache {

  def main(args: Array[String]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global


    println(new File("x") == new File("x"))

    val sourceFile = new File("../jartree/target/jartree.jar")

    val source = () => new FileInputStream(sourceFile)

    val cacheDir = new File("../jartree/target/jarcache")

    val cache = JarCache(cacheDir)

    val hash = JarCache.calculateHash(source)

    val file = cache.get(hash, source)

    file.foreach({ file =>
      println(file.getAbsolutePath)
      require(JarCache.calculateHash(() => new FileInputStream(file)).sameElements(hash))
    })






  }

}
