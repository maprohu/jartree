package jartree

import jartree.JarTree._
//import org.reactivestreams.Processor

import scala.collection.immutable._
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}

/**
  * Created by martonpapp on 27/08/16.
  */
object JarTree {
  type JarHash = Seq[Byte]
  type Unresolved = Seq[JarKey]
  type ResolutionResult[T] = Either[Unresolved, T]
  type ResolutionResultAsync[T] = Future[Either[Unresolved, T]]

//  def requestToKey(request: ClassLoaderRequest) : ClassLoaderKey = {
//    ClassLoaderKey(
//      jar = request.jar,
//      parents = request.parents.map(requestToKey)
//    )
//  }

  def toJarCacheHash(hash: JarHash) : JarCache.Hash = {
    hash.toArray
  }

//  def apply(
//    parentClassLoader: ClassLoader,
//    cache: JarCache
//  ): JarTree = new JarTree(parentClassLoader, cache)

  def apply(
    parentClassLoader: ClassLoader,
    resolver: JarResolver
  ): JarTree = new JarTree(parentClassLoader, resolver)


  def collectMavenParents(cl: ClassLoaderKey) : Seq[ClassLoaderKey] = {
    cl.parents.flatMap({ p =>
      p +: collectMavenParents(p)
    })
  }

  def flatten(cl: ClassLoaderKey) : ClassLoaderKey = {
    val sch = new org.eclipse.aether.util.version.GenericVersionScheme()

    val grouped =
      collectMavenParents(cl)
        .map({ pcl =>
          pcl.jar match {
            case mvn : MavenJarKey =>
              val key = (mvn.groupId, mvn.artifactId, mvn.packaging, mvn.classifier)
              val version = sch.parseVersion(mvn.version)
              key -> (version, pcl)
            case hash : HashJarKey =>
              //          val name = JarCache.hashToString(hash.hash.toArray)
              //          val key = (name, name, "jar", None)
              val key = hash.hash
              val version = sch.parseVersion("1.0")
              key -> (version, pcl)
          }
        })
        .groupBy({ case (key, (version, cl)) => key })
        .values

    val newParents =
      grouped
        .map({ group =>
          group
            .map({ case (key, (version, cl)) => (version, cl) })
            .maxBy({ case (version, cl) => version })
        })
        .map({ case (version, cl ) => cl })
        .to[Seq]

    cl.copy(parents = newParents)

    //        .toSeq
    //        .map(_._2)
    //        .map(_.maxBy(_._1))

    //    cl.parents.map({ p =>
    //      p.jar match {
    //        case m : MavenJarKey =>
    //          sch.parseVersion(m.version)
    //          m.version
    //      }
    //    })

  }

//  val threadLocal = new ThreadLocal[JarTree]()
}

class JarTree(
  val parentClassLoader: ClassLoader,
  val resolver: JarResolver
) {

  val classLoaderMap = mutable.WeakHashMap.empty[ClassLoaderKey, ResolutionResultAsync[JarTreeClassLoader]]


//  def get(
//    request: ClassLoaderRequest
//  )(implicit
//    executionContext: ExecutionContext
//  ) : Future[Either[Seq[JarKey], ClassLoader]] = {
//
//    val key = requestToKey(request)
//
//    get(request, key)
//  }

//  def get(
//    request: ClassLoaderRequest,
//    key: ClassLoaderKey
//  )(implicit
//    executionContext: ExecutionContext
//  ) : Future[Either[Seq[JarKey], ClassLoader]] = {
//    val producer = synchronized {
//      classLoaderMap
//        .get(key)
//        .map(f => () => f )
//        .getOrElse({
//          val promise = Promise[ClassLoader]()
//
//          classLoaderMap.update(key, promise.future)
//
//          { () =>
//
//            val source = request.jar.source match {
//              case URLJarSource(url) =>
//                val u = new URL(url)
//                () => u.openStream()
//            }
//
//            val jarFuture = resolver.resolve(
//              toJarCacheHash(request.jar.hash),
//              source
//            )
//            val parentsFuture = Future.sequence(
//              request.parents
//                .zip(key.parents)
//                .map({
//                  case (pRequest, pKey) =>
//                    get(pRequest, pKey)
//                })
//            )
//
//            promise.completeWith(
//              for {
//                jar <- jarFuture
//                parents <- parentsFuture
//              } yield {
//                new JarTreeClassLoader(
//                  jar.toURI.toURL,
//                  parents,
//                  parentClassLoader
//                )
//              }
//            )
//
//            promise.future
//          }
//        })
//    }
//
//    producer()
//  }

  def get(
    key: ClassLoaderKey
  )(implicit
    executionContext: ExecutionContext
  ) : ResolutionResultAsync[JarTreeClassLoader] = {
    val producer = synchronized {
      classLoaderMap
        .get(key)
        .map(f => () => f )
        .getOrElse({
          val promise = Promise[ResolutionResult[JarTreeClassLoader]]()

          classLoaderMap.update(key, promise.future)

          { () =>

//            val source = request.jar.source match {
//              case URLJarSource(url) =>
//                val u = new URL(url)
//                () => u.openStream()
//            }

            val jarFuture = resolver.resolve(
              key.jar
            )
            val parentsFuture = Future.sequence(
              key.parents
                .map({ parent =>
                  get(parent)
                })
            )

            promise.completeWith(
              for {
                jar <- jarFuture
                parents <- parentsFuture
              } yield {
                val jarEither =
                  jar
                    .map({ url =>
                      Right(url)
                    })
                    .getOrElse(
                      Left(Seq(key.jar))
                    )

                val missing : Seq[JarKey] =
                  (jarEither +: parents)
                    .flatMap(_.left.toSeq.flatten)
                    .distinct


                if (missing.isEmpty) {
                  Right(
                    new JarTreeClassLoader(
                      jarEither.right.get.toURI.toURL,
                      parents.map(_.right.get),
                      parentClassLoader
                    )
                  )
                } else {
                  Left(missing)
                }
              }
            )

            promise.future
          }
        })
    }

    producer()
  }

  def run[T](
    request: RunRequest,
    runner: T => Unit
//    processor: Processor[Array[Byte], Array[Byte]]
  )(implicit
    executionContext: ExecutionContext
  ) : ResolutionResultAsync[Unit] = {
    for {
      maybeCl <- get(request.classLoader)
    } yield {
      maybeCl
        .fold(
          Left(_),
          { cl =>
            val runClass = cl.loadClass(request.className)
//            val method = runClass.getMethod(
//              request.methodName,
//              classOf[Processor[_, _]]
//            )

            val instance = runClass.newInstance().asInstanceOf[T]

            runner(instance)
//            try {
//              threadLocal.set(this)
//
//              method.invoke(
//                runClass.newInstance(),
//                processor
//              )
//            } finally {
//              threadLocal.remove()
//            }
            Right()
          }
        )
    }
  }


}

case class ClassLoaderKey(
  jar: JarKey,
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

//case class ClassLoaderRequest(
//  jar: JarKey,
//  parents: Seq[ClassLoaderRequest]
//)

case class RunRequest(
  classLoader: ClassLoaderKey,
  className: String
//  methodName: String
)



