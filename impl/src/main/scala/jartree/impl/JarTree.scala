package jartree.impl

import java.io.InputStream

import jartree._
import jartree.impl.JarTree.{ResolutionResult, ResolutionResultAsync}
import jartree.util.{CaseClassLoaderKey, CaseJarKey}

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

//  def toJarCacheHash(hash: JarHash) : JarCache.Hash = {
//    hash.toArray
//  }

//  def apply(
//    parentClassLoader: ClassLoader,
//    cache: JarCache
//  ): JarTree = new JarTree(parentClassLoader, cache)

  def apply(
    parentClassLoader: ClassLoader,
    resolver: JarResolver
  )(implicit
    executionContext: ExecutionContext
  ): JarTree = new JarTree(parentClassLoader, resolver)


  def collectMavenParents(cl: CaseClassLoaderKey) : Seq[CaseClassLoaderKey] = {
    cl.dependenciesSeq.flatMap({ p =>
      p +: collectMavenParents(p)
    })
  }

  def flatten(cl: CaseClassLoaderKey) : CaseClassLoaderKey = {
    val sch = new org.eclipse.aether.util.version.GenericVersionScheme()

    val grouped =
      collectMavenParents(cl)
        .map({ pcl =>
          pcl.jar match {
            case mvn : MavenJarKey =>
              val key = (mvn.groupId, mvn.artifactId, mvn.classifier)
              val version = sch.parseVersion(mvn.version)
              key -> (version, pcl)
            case hash : HashJarKey =>
              val key = hash.hash
              val version = sch.parseVersion("1.0")
              key -> (version, pcl)
          }
        })
        .groupBy({ case (key, (version, cl)) => key })
        .values

    val newDependencies =
      grouped
        .map({ group =>
          group
            .map({ case (key, (version, cl)) => (version, cl) })
            .maxBy({ case (version, cl) => version })
        })
        .map({ case (version, cl ) => cl })
        .to[Seq]

    cl.copy(dependenciesSeq = newDependencies)

  }

//  val threadLocal = new ThreadLocal[JarTree]()
}

class JarTree(
  val parentClassLoader: ClassLoader,
  val resolver: JarResolver
)(implicit
    executionContext: ExecutionContext
) extends JarTreeRunner {

  val classLoaderMap = mutable.WeakHashMap.empty[CaseClassLoaderKey, ResolutionResultAsync[JarTreeClassLoader]]

  def clear() = synchronized {
    classLoaderMap.clear()
  }

  def get(
    key: CaseClassLoaderKey
  ) : ResolutionResultAsync[JarTreeClassLoader] = {
    val producer = synchronized {
      classLoaderMap
        .get(key)
        .map(f => () => f )
        .getOrElse({
          val promise = Promise[ResolutionResult[JarTreeClassLoader]]()

          classLoaderMap.update(key, promise.future)

          { () =>

            val jarOption = resolver.resolve(
              key.jar
            )
            val jarFuture = jarOption.map({ f =>
              f.map(f => Some(f))
            }).getOrElse(
              Future.successful(None)
            )
            val parentsFuture = Future.sequence(
              key.dependenciesSeq
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


  def runInternal[T](
    request: RunRequest
  ) = {
    for {
      maybeCl <- get(CaseClassLoaderKey(request.classLoader))
    } yield {
      maybeCl
        .fold(
          Left(_),
          { cl =>
            val runClass = cl.loadClass(request.className)

            val instance = runClass.newInstance().asInstanceOf[T]

            Right(instance)
          }
        )
    }
  }

  def run[T](
    request: RunRequest,
    callback: JarTreeRunnerCallback[T]
  ) : Unit = {
    val result = runInternal[T](request)

    result.onFailure({
      case ex : Throwable =>
        callback.failure(ex)
    })

    import scala.collection.JavaConversions._
    result.onSuccess({
      case Right(instance) =>
        callback.resolved(instance)
      case Left(missing) =>
        callback.unresolved(missing)
    })
  }

  override def cacheJar(jarKey: JarKey): JarCacheLoader = {
    val promise = Promise[InputStream]()

    resolver.cache.put(
      CaseJarKey(jarKey),
      promise.future
    )

    new JarCacheLoader {
      override def failure(ex: Throwable): Unit = promise.failure(ex)
      override def success(is: InputStream): Unit = promise.success(is)
    }
  }
}



