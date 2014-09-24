package xyz.seto.obscene.utils


class Rectangle(val cords: Point, val dims: Point) {
  def this(x: Float, y: Float, width: Float, height: Float) = this(new Point(x, y), new Point(width, height))
  def this(width: Float, height: Float) = this(0, 0, width, height)

  val topCorner: Point = cords + dims

  private def scaleDims(p: Point): Point = topCorner.scaleUp(p) - cords

  def union(p: Point): Rectangle = new Rectangle(cords.scaleDown(p), scaleDims(p))

  def union(r: Rectangle): Rectangle = union(r.cords).union(r.topCorner)

  def union(cX: Float, cY: Float, cWidth: Float, cHeight: Float) = {}

  def translate(x: Float, y: Float): Rectangle = translate(new Point(x, y))

  def translate(p: Point): Rectangle = new Rectangle(cords + p, dims)

}
