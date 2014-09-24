package xyz.seto.obscene.utils


class Point(val x: Float, val y: Float) {
  def this() = this(0, 0)

  def >(that: Point): Boolean = (that.x > this.x) || (that.y > this.y)

  def <(that: Point): Boolean = !(that > this)

  def +(that: Point): Point = new Point(that.x + this.x, that.y + this.y)

  def -(that: Point): Point = -that + this

  def unary_- : Point = new Point(-this.x, -this.y)

  def scaleUp(that: Point) = scale(that, larger)

  def scaleDown(that: Point) = scale(that, smaller)

  private def larger(f1: Float, f2: Float): Float = if (f1 > f2) f1 else f2

  private def smaller(f1:Float, f2: Float): Float = if (f1 < f2) f1 else f2

  private def scale(that: Point, f: (Float, Float) => Float): Point = new Point(f(that.x, this.x), f(that.y, this.y))

}


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
