package xyz.seto.obscene.utils;

class OrientedBoundingBox(val angle: Float, val centerX: Float, val CenterY: Float, val width: Float, val height: Float) {
  val ratio = width/height
  val squareness = if(ratio > 1) 1 / ratio else ratio
}
