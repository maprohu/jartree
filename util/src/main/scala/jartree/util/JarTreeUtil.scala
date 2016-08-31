package jartree.util

import jartree.{ClassLoaderKey, HashJarKey, JarKey, MavenJarKey}

import scala.collection.immutable._
import scala.collection.JavaConversions._

/**
  * Created by pappmar on 31/08/2016.
  */

sealed trait CaseJarKey extends JarKey

object CaseJarKey {
  def apply(
    jarKey: JarKey
  ) = {
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


case class CaseClassLoaderKey(
  jar: CaseJarKey,
  dependencies: Seq[CaseClassLoaderKey]
)

object CaseClassLoaderKey {
  def apply(clk: ClassLoaderKey) : CaseClassLoaderKey = apply(
    CaseJarKey(clk.jar),
    clk.dependencies.map(clk => CaseClassLoaderKey(clk)).to[Seq]
  )
}