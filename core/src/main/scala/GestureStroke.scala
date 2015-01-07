package xyz.seto.obscene

import xyz.seto.obscene.utils.Rectangle

import java.io.DataInput
import java.io.DataOutput

import scala.collection.mutable.ListBuffer


/** A Single stroke of a gesture
 * Comprised of a list of gesture points and
 * the rectangle bounding box
 */
class GestureStroke(val points: List[GesturePoint]) {
  private def createBoundingBox(cBox: Rectangle, cPoints: List[GesturePoint]): Rectangle = cPoints match {
      case cP :: cPoints => createBoundingBox(cBox.union(cP), cPoints)
      case Nil => cBox
  }

  /** The rectangle bounding the stroke dictated by the lower and highest GesturePoint */
  lazy val boundingBox: Rectangle = createBoundingBox(new Rectangle(0, 0), points)

  /** The diagonal of the stoke's bounding box */
  lazy val length: Float = this.boundingBox.diagonal

  /** Flattens the all the points in a gesture stroke
   * @return A list of Floats formatted as X1, Y1, ... Xn, Yn
   */
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

  /** Serialize the stroke to the given data stream
   * writes the number of points as an integer and then
   * serializes all contained points
   * @param stream The stream to write data to
   */
  def serialize(stream: DataOutput) {
    stream.writeInt(this.points.length)
    this.points.map(x => x.serialize(stream))
  }
}

object GestureStroke {
  /** Deserialize a stroke from the given DataInput
   * Reads the number of points, an integer, and then that
   * many points
   * @param stream The DataInput to read from
   */
  def deserialize(stream: DataInput): GestureStroke =
    new GestureStroke((for(_ <- 1 to stream.readInt())
      yield GesturePoint.deserialize(stream)).to[List])
}
