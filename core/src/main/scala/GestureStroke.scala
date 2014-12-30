package xyz.seto.obscene

import xyz.seto.obscene.utils.Rectangle

import java.io.DataOutputStream;
import java.io.DataInputStream;

import scala.collection.mutable.ListBuffer


class GestureStroke(val points: List[GesturePoint]) {
  private def createBoundingBox(cBox: Rectangle, cPoints: List[GesturePoint]): Rectangle = cPoints match {
      case cP :: cPoints => createBoundingBox(cBox.union(cP), cPoints)
      case Nil => cBox
  }

  val boundingBox: Rectangle = createBoundingBox(new Rectangle(0, 0), points)

  val length: Float = this.boundingBox.diagonal

  def flatPoints: List[Float] = {
    def loop(points: List[GesturePoint], flattened: ListBuffer[Float]): List[Float] = points match {
        case p1 :: ps => {
          flattened.append(p1.x)
          flattened.append(p1.y)
          loop(ps, flattened)
        }
        case _ => flattened.toList
    }
    loop(points, ListBuffer())
  }

  def serialize(stream: DataOutputStream) {
    stream.writeInt(this.points.length)
    this.points.map(x => x.serialize(stream))
  }
}

object GestureStroke {
  def deserialize(stream: DataInputStream): GestureStroke =
    new GestureStroke((for(_ <- 1 to stream.readInt())
      yield GesturePoint.deserialize(stream)).to[List])
}

