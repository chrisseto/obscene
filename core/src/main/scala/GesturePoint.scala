package xyz.seto.obscene

class GesturePoint(val x: Float, val y: Float, val timeStamp: Long) {
  override def toString: String = s"<GesturePoint ($x, $y) $timeStamp>"
}
