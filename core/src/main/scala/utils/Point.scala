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

