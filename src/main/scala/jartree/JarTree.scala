package jartree

import java.io.{InputStream, OutputStream}
import java.net.URL

import jartree.JarTree._
import org.reactivestreams.Processor

import scala.collection.immutable._
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}

/**
  * Created by martonpapp on 27/08/16.
  */
object JarTree {
  type JarHash = Seq[Byte]

  def requestToKey(request: ClassLoaderRequest) : ClassLoaderKey = {
    ClassLoaderKey(
      jar = request.jar.hash,
      parents = request.parents.map(requestToKey)
    )
  }

  def toJarCacheHash(hash: JarHash) : JarCache.Hash = {
    hash.toArray
  }

//  def apply(
//    parentClassLoader: ClassLoader,
//    cache: JarCache
//  ): JarTree = new JarTree(parentClassLoader, cache)


}

class JarTree(
  parentClassLoader: ClassLoader,
  resolver: JarResolver
) {

  val classLoaderMap = mutable.WeakHashMap.empty[ClassLoaderKey, Future[ClassLoader]]

  def get(
    request: ClassLoaderRequest
  )(implicit
    executionContext: ExecutionContext
  ) : Future[Either[Seq[JarKey], ClassLoader]] = {

    val key = requestToKey(request)

    get(request, key)
  }

  def get(
    request: ClassLoaderRequest,
    key: ClassLoaderKey
  )(implicit
    executionContext: ExecutionContext
  ) : Future[Either[Seq[JarKey], ClassLoader]] = {
    val producer = synchronized {
      classLoaderMap
        .get(key)
        .map(f => () => f )
        .getOrElse({
          val promise = Promise[ClassLoader]()

          classLoaderMap.update(key, promise.future)

          { () =>

            val source = request.jar.source match {
              case URLJarSource(url) =>
                val u = new URL(url)
                () => u.openStream()
            }

            val jarFuture = cache.get(
              toJarCacheHash(request.jar.hash),
              source
            )
            val parentsFuture = Future.sequence(
              request.parents
                .zip(key.parents)
                .map({
                  case (pRequest, pKey) =>
                    get(pRequest, pKey)
                })
            )

            promise.completeWith(
              for {
                jar <- jarFuture
                parents <- parentsFuture
              } yield {
                new JarTreeClassLoader(
                  jar.toURI.toURL,
                  parents,
                  parentClassLoader
                )
              }
            )

            promise.future
          }
        })
    }

    producer()
  }

  def run(
    request: RunRequest,
    processor: Processor[Array[Byte], Array[Byte]]
  )(implicit
    executionContext: ExecutionContext
  ) : Future[Unit] = {
    for {
      cl <- get(request.classLoader)
    } yield {
      val runClass = cl.loadClass(request.className)
      val method = runClass.getMethod(
        request.methodName,
        classOf[Processor[_, _]]
      )
      method.invoke(
        runClass.newInstance(),
        processor
      )
    }

  }


}

case class ClassLoaderKey(
  jar: JarHash,
  parents: Seq[ClassLoaderKey]
)

case class JarRequest(
  hash: JarHash,
  source: JarSource
)

sealed trait JarSource
case class URLJarSource(
  url: String
) extends JarSource

case class ClassLoaderRequest(
  jar: JarRequest,
  parents: Seq[ClassLoaderRequest]
)

case class RunRequest(
  classLoader: ClassLoaderRequest,
  className: String,
  methodName: String
)



