package jartree

import java.net.URL

import org.jboss.shrinkwrap.resolver.api.maven.Maven

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
  * Created by martonpapp on 28/08/16.
  */
class JarResolver(cache: JarCache) {

  def resolve(
    key: JarKey
  )(implicit
    executionContext: ExecutionContext
  ) : Future[Option[URL]] = {

    key match {
      case HashJarKey(hash) =>
        cache.maybeGet(
          JarTree.toJarCacheHash(hash)
        ).map({ fileFuture =>
          fileFuture.map({ file =>
            Some(
              file.toURI.toURL
            )
          })
        }).getOrElse(
          Future.successful(None)
        )
      case mvn : MavenJarKey =>
        Future {
          Try({
            import mvn._
            val items =
              Seq(groupId, artifactId) ++
              packaging.toSeq ++
              classifier.toSeq ++
              Seq(version)

            Maven
              .resolver()
              .resolve(items.mkString(":"))
              .withoutTransitivity()
              .asSingleFile()
              .toURI
              .toURL
          }).toOption
        }
    }

  }

}

object JarResolver {

}

sealed trait JarKey

case class HashJarKey(
  hash: JarTree.JarHash
) extends JarKey

case class MavenJarKey(
  groupId: String,
  artifactId: String,
  version: String,
  packaging : Option[String] = None,
  classifier: Option[String] = None
)

