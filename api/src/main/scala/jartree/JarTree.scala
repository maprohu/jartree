package jartree


/**
  * Created by martonpapp on 27/08/16.
  */

trait JarKey

trait HashJarKey extends JarKey {
  def hash() : Array[Byte]
}

trait MavenJarKey extends JarKey {
  def groupId() : String
  def artifactId() : String
  def version() : String
  def classifier() : String
}


trait  ClassLoaderKey {
  def jar(): JarKey
  def dependencies(): java.util.Collection[ClassLoaderKey]
}

trait RunRequest {
  def classLoader(): ClassLoaderKey
  def className(): String
}

trait JarTreeRunner {
  def run[T](
    request: RunRequest,
    callback: JarTreeRunnerCallback[T]
  ) : Unit
}

trait JarTreeRunnerCallback[T] {
  def resolved(instance: T) : Unit
  def unresolved(jars: java.util.Collection[JarKey]) : Unit
  def failure(ex: Throwable) : Unit
}


