package jartree.impl

import java.net.URL

import com.typesafe.scalalogging.LazyLogging
import jartree.util.{CaseJarKey, HashJarKeyImpl, MavenJarKeyImpl}
import org.jboss.shrinkwrap.resolver.api.maven.Maven

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Try}

/**
  * Created by martonpapp on 28/08/16.
  */
class JarResolver(
  val cache: JarCache
) extends LazyLogging {

  def resolve(
    key: CaseJarKey
  )(implicit
    executionContext: ExecutionContext
  ) : Option[Future[URL]] = {

    cache.maybeGet(
      key
    ).map({ ff =>
      ff.map(_.toURI.toURL)
    }).orElse({
      key match {
        case h : HashJarKeyImpl =>
          None
        case mvn : MavenJarKeyImpl =>
          Try({
            Future {
              import mvn._
              val items =
                Seq(groupId, artifactId) ++
                  classifier.toSeq ++
                  Seq(version)

              Maven
                .resolver()
                .resolve(items.mkString(":"))
                .withoutTransitivity()
                .asSingleFile()
                .toURI
                .toURL
            }
          }).recoverWith({
            case ex =>
              logger.error("error resolving jar", ex)
              Failure(ex)
          }).toOption

      }

    })


  }

}

object JarResolver {

  def apply(cache: JarCache): JarResolver = new JarResolver(cache)

}

//case class MavenJarKey(
//  groupId: String,
//  artifactId: String,
//  version: String,
//  packaging : String = PackagingType.JAR.getId,
//  classifier: Option[String] = None
//) extends JarKey {
//  def toMavenCoordinate : MavenCoordinate = {
//    MavenCoordinates.createCoordinate(
//      groupId,
//      artifactId,
//      version,
//      PackagingType.of(packaging),
//      classifier.getOrElse("")
//    )
//  }
//}

//object MavenJarKey {
//  def apply(canonical: String) : MavenJarKey = {
//    val c = MavenCoordinates.createCoordinate(canonical)
//
//    MavenJarKey.apply(
//      groupId = c.getGroupId,
//      artifactId = c.getArtifactId,
//      version = c.getVersion,
//      packaging = c.getPackaging.getId,
//      classifier = Option(c.getClassifier).filterNot(_.isEmpty)
//    )
//  }
//}

