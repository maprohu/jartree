package jartree

import java.net.{URL, URLClassLoader}


/**
  * Created by martonpapp on 26/08/16.
  */



class JarTreeClassLoader(
  url: URL,
  deps: Seq[ClassLoader],
  parent: ClassLoader
) extends ClassLoader(
  parent
) {
  val publicParent = new SequenceClassLoader(
    deps,
    parent
  )
  private val urlClassLoader = new URLClassLoader(Array(url), null) {
    def findClassPublic(name: String): Class[_] = {
      findClass(name)
    }
    override def findClass(name: String): Class[_] = {
      try {
        super.findClass(name)
      } catch {
        case _ : ClassNotFoundException =>
          publicParent.loadClass(name)
      }
    }
  }
  override def loadClass(name: String, resolve: Boolean): Class[_] = {
    try {
      urlClassLoader.findClassPublic(name)
    } catch {
      case _ : ClassNotFoundException =>
        super.loadClass(name, resolve)
    }
  }
}

class PublicClassLoader(cl: ClassLoader) extends ClassLoader(cl) {
  def loadClassPublic(name: String, resolve: Boolean): Class[_] = super.loadClass(name, resolve)
  def findClassPublic(name: String): Class[_] = super.findClass(name)
}

class SequenceClassLoader(
  classLoaders: Seq[ClassLoader],
  parent: ClassLoader
) extends ClassLoader(parent) {


  private val cls = classLoaders
    .map({ cl =>
      new PublicClassLoader(cl)
    })
    .toStream

  override def loadClass(name: String, resolve: Boolean): Class[_] = {
    cls
      .map({ cl =>
        try {
          Some(cl.loadClassPublic(name, resolve))
        } catch {
          case _ : ClassNotFoundException =>
            None
        }
      })
      .find(_.isDefined)
      .map(_.get)
      .getOrElse({
        super.loadClass(name, resolve)
      })
  }
}
