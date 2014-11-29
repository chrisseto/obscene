package xyz.seto.obscene

import xyz.seto.obscene.utils.Point

import java.io.DataOutputStream;
import java.io.DataInputStream;


class GesturePoint(val x: Float, val y: Float, val timeStamp: Long) {
  def point = new Point(x, y)

  def serialize(stream: DataOutputStream) = {
    stream.writeFloat(this.x)
    stream.writeFloat(this.y)
    stream.writeLong(this.timeStamp)
  }

  override def toString: String = s"<GesturePoint ($x, $y) $timeStamp>"
}

object GesturePoint {
  def deserialize(stream: DataInputStream): GesturePoint =
    new GesturePoint(
      stream.readFloat(), stream.readFloat(), stream.readLong())
}
