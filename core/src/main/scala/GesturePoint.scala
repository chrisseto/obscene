package xyz.seto.obscene

import java.io.DataInput
import java.io.DataOutput

import xyz.seto.obscene.utils.Point


/** A represetation of a single point in a gesture
 * comprised of a point, X, Y, and a timestamp, in milliseconds, of when that point was made
 * relative to the start up of the application
 * timestamp just needs to be computed the same way everytime
 */
class GesturePoint(override val x: Float, override val y: Float, val timeStamp: Long) extends Point(x, y) {
  /** Write a GesturePoint to the specifed steam as x, y, timestamp
   * Float, Float, Long
   * @param stream The Output to write to
   */
  def serialize(stream: DataOutput) = {
    stream.writeFloat(this.x)
    stream.writeFloat(this.y)
    stream.writeLong(this.timeStamp)
  }

  override def toString: String = s"<GesturePoint ($x, $y) $timeStamp>"
}

object GesturePoint {
  /** Create a GesturePoint from a DataInput
   *
   * @param stream The DataInput containing the serialized data
   * @return The deserialized GesturePoint
   */
  def deserialize(stream: DataInput): GesturePoint =
    new GesturePoint(
      stream.readFloat(),
      stream.readFloat(),
      stream.readLong()
    )
}
