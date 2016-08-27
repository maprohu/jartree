package jartree

import java.io.{File, FileOutputStream, InputStream, OutputStream}
import java.security.{DigestInputStream, DigestOutputStream, MessageDigest}

import jartree.JarCache._
import org.apache.commons.codec.binary.Base64
import org.apache.commons.io.IOUtils

import scala.collection.immutable._
import scala.concurrent.{Future, Promise}
import scala.util.Try

/**
  * Created by martonpapp on 27/08/16.
  */
class JarCache(root: File) {

  root.mkdirs()

  var locked = Map.empty[File, Future[File]]

  def delete(file: File) = synchronized {
    file.delete()
  }

  def get(hash: Hash, source: Source) : Future[File] = {
    val fileName = s"${hashToString(hash)}.jar"
    val jarFile = new File(root, fileName)

    val producer = synchronized {
      locked
        .get(jarFile)
        .map(future => () => future)
        .getOrElse({
          if (jarFile.exists()) {
            () => Future.successful(jarFile)
          } else {
            val promise = Promise[File]()

            locked = locked.updated(jarFile, promise.future)

            { () =>
              promise.complete(
                Try {
                  try {
                    val in = source()
                    val digestStream = createDigestInputStream(in)
                    val out = new FileOutputStream(jarFile)
                    try {
                      IOUtils.copy(digestStream, out)
                    } finally {
                      IOUtils.closeQuietly(digestStream, out)
                    }
                    require(digestStream.getMessageDigest.digest().sameElements(hash), "digest mismatch for jar")
                    jarFile
                  } catch {
                    case ex : Throwable =>
                      delete(jarFile)
                      throw ex
                  }
                }
              )

              Future.successful(jarFile)
            }

          }
        })
    }

    producer()

  }

}

object JarCache {

  type Hash = Array[Byte]
  type Source = () => InputStream

  def createDigest = {
    MessageDigest.getInstance("SHA-256")
  }

  def createDigestInputStream(is: InputStream) = {
    new DigestInputStream(is, createDigest)
  }

  def calculateHash(source: Source) : Hash = {
    val in = source()
    try {
      val digestInputStream = createDigestInputStream(in)
      IOUtils.copy(digestInputStream, new OutputStream {
        override def write(i: Int): Unit = ()
      })
      digestInputStream.getMessageDigest.digest()
    } finally {
      IOUtils.closeQuietly(in)
    }
  }

  def hashToString(hash: Hash) : String = {
    Base64.encodeBase64URLSafeString(hash)
  }

  def apply(root: File): JarCache = new JarCache(root)

}
