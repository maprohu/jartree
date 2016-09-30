package jartree.util

import java.io.File
import java.util

import jartree.{ClassLoaderKey, HashJarKey, JarKey, MavenJarKey}
import org.jboss.shrinkwrap.resolver.api.maven.coordinate.MavenCoordinates

import scala.collection.immutable._
import scala.collection.JavaConversions._

/**
  * Created by pappmar on 31/08/2016.
  */

sealed trait CaseJarKey extends JarKey

object CaseJarKey {
  def apply(
    jarKey: JarKey
  ) : CaseJarKey = {
    jarKey match {
      case k : HashJarKey =>
        HashJarKeyImpl(
          k.hash().to[Seq]
        )
      case k : MavenJarKey =>
        MavenJarKeyImpl(
          k.groupId(),
          k.artifactId(),
          k.version(),
          Option(k.classifier()).filterNot(_.isEmpty)
        )
      case _ => ???
    }
  }
}

case class HashJarKeyImpl(
  hashSeq : Seq[Byte]
) extends CaseJarKey with HashJarKey {
  override val hash: Array[Byte] = hashSeq.toArray
}

case class MavenJarKeyImpl(
  groupId: String,
  artifactId: String,
  version: String,
  classifierOpt: Option[String]
) extends CaseJarKey with MavenJarKey {
  override def classifier(): String = classifierOpt.getOrElse("")
}

object MavenJarKeyImpl {

  def apply(canonical: String) : MavenJarKeyImpl = {
    val mc = MavenCoordinates.createCoordinate(canonical)

    MavenJarKeyImpl(
      mc.getGroupId,
      mc.getArtifactId,
      mc.getVersion,
      Option(mc.getClassifier).filterNot(_.isEmpty)
    )
  }

}

//case class FileJarKeyImpl(
//  file: File
//) extends CaseJarKey


case class CaseClassLoaderKey(
  jar: CaseJarKey,
  dependenciesSeq: Seq[CaseClassLoaderKey]
) extends ClassLoaderKey {
  override def dependencies(): util.Collection[ClassLoaderKey] = dependenciesSeq
}

object CaseClassLoaderKey {
  def apply(clk: ClassLoaderKey) : CaseClassLoaderKey = apply(
    CaseJarKey(clk.jar),
    clk.dependencies.map(clk => CaseClassLoaderKey(clk)).to[Seq]
  )
}

case class RunRequestImpl(
  classLoader: CaseClassLoaderKey,
  className: String
) extends jartree.RunRequest

object RunRequestImpl {
  def apply(
    classLoaderKey: ClassLoaderKey,
    className: String
  ) = new RunRequestImpl(CaseClassLoaderKey(classLoaderKey), className)
}