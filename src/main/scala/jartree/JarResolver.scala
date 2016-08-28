package jartree

import java.net.URL

import org.jboss.shrinkwrap.resolver.api.maven.{Maven, PackagingType}
import org.jboss.shrinkwrap.resolver.api.maven.coordinate.{MavenCoordinate, MavenCoordinates}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Try}

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
              Seq(groupId, artifactId, packaging) ++
              classifier.toSeq ++
              Seq(version)

            Maven
              .resolver()
              .resolve(items.mkString(":"))
              .withoutTransitivity()
              .asSingleFile()
              .toURI
              .toURL
          }).recoverWith({
            case ex =>
              ex.printStackTrace()
              Failure(ex)
          }).toOption
        }
    }

  }

}

object JarResolver {

  def apply(cache: JarCache): JarResolver = new JarResolver(cache)

}

sealed trait JarKey

case class HashJarKey(
  hash: JarTree.JarHash
) extends JarKey

case class MavenJarKey(
  groupId: String,
  artifactId: String,
  version: String,
  packaging : String = PackagingType.JAR.getId,
  classifier: Option[String] = None
) extends JarKey {
  def toMavenCoordinate : MavenCoordinate = {
    MavenCoordinates.createCoordinate(
      groupId,
      artifactId,
      version,
      PackagingType.of(packaging),
      classifier.getOrElse("")
    )
  }
}

object MavenJarKey {
  def apply(canonical: String) : MavenJarKey = {
    val c = MavenCoordinates.createCoordinate(canonical)

    MavenJarKey.apply(
      groupId = c.getGroupId,
      artifactId = c.getArtifactId,
      version = c.getVersion,
      packaging = c.getPackaging.getId,
      classifier = Option(c.getClassifier).filterNot(_.isEmpty)
    )
  }
}

