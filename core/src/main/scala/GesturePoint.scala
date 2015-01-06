package xyz.seto.obscene

import java.io.DataInput
import java.io.DataOutput

import xyz.seto.obscene.utils.Point


class GesturePoint(override val x: Float, override val y: Float, val timeStamp: Long) extends Point(x, y) {
  def serialize(stream: DataOutput) = {
    stream.writeFloat(this.x)
    stream.writeFloat(this.y)
    stream.writeLong(this.timeStamp)
  }

  override def toString: String = s"<GesturePoint ($x, $y) $timeStamp>"
}

object GesturePoint {
  def deserialize(stream: DataInput): GesturePoint =
    new GesturePoint(
      stream.readFloat(), stream.readFloat(), stream.readLong())
}
