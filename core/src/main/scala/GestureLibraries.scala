package xyz.seto.obscene

import xyz.seto.obscene.GestureConstants._

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.lang.ref.WeakReference;

object GestureLibraries {
  def fromFile(path: String): GestureLibrary = fromFile(new File(path))

  def fromFile(path: File): GestureLibrary = new FileGestureLibrary(path)

  class FileGestureLibrary(val file: File) extends GestureLibrary {
    val parent = file.getParentFile()
    override def isReadOnly: Boolean = !file.canWrite()

    def save: Boolean = {
      if (!mStore.hasChanged)
        true
      else
        if (!parent.exists() && !parent.mkdirs())
          false
        else
          try {
            file.createNewFile()
            mStore.save(new FileOutputStream(file), true)
            true
          } catch {
            case e: FileNotFoundException => false
            case e: IOException => false
          }

    }

    def load: Boolean =
      if (file.exists() && file.canRead())
        try {
          mStore.load(new FileInputStream(file), true)
          true
        } catch {
          case e: FileNotFoundException => false
          case e: IOException => false
        }
        else false
  }

}
