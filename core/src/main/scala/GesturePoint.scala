package xyz.seto.obscene

import xyz.seto.obscene.utils.Point


class GesturePoint(val x: Float, val y: Float, val timeStamp: Long) {
  def point = new Point(x, y)

  def serialize():(Float, Float, Long) = (x, y, timeStamp)

  override def toString: String = s"<GesturePoint ($x, $y) $timeStamp>"
}
