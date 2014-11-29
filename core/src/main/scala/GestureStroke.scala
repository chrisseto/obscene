package xyz.seto.obscene

import xyz.seto.obscene.utils.Rectangle

import java.io.DataOutputStream;
import java.io.DataInputStream;


class GestureStroke(val points: List[GesturePoint]){
  private def createBoundingBox(cBox: Rectangle, cPoints: List[GesturePoint]): Rectangle = cPoints match {
      case cP :: cPoints => createBoundingBox(cBox.union(cP.point), cPoints)
      case Nil => cBox
  }

  val boundingBox: Rectangle = createBoundingBox(new Rectangle(0, 0), points)

  val length: Float = this.boundingBox.diagonal

  def flatPoints: Array[Float] = {
    def loop(points: List[GesturePoint], flattened: List[Float]): List[Float] = points match {
        case p1 :: ps => flattened :+ p1.x :+ p1.y
        case _ => flattened
    }
    loop(points, List()).toArray
  }

  def serialize(stream: DataOutputStream) {
    stream.write(this.points.length)
    this.points.map(x => x.serialize(stream))
  }
}

