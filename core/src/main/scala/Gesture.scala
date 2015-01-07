package xyz.seto.obscene

import java.io.DataOutput
import java.io.DataInput

import xyz.seto.obscene.utils.Rectangle


class Gesture(val id: Long, val strokes: List[GestureStroke], val boundingBox: Rectangle){
  def this() = this(0, List())
  def this(id: Long, strokes: List[GestureStroke]) =
    this(id, strokes, Gesture.computeBoundingBox(strokes))

  def addStroke(stroke: GestureStroke): Gesture =
    new Gesture(id, strokes :+ stroke, boundingBox.union(stroke.boundingBox))

  def serialize(stream: DataOutput): Unit = {
    stream.writeLong(this.id)
    stream.writeInt(this.strokes.length)
    this.strokes.map(x => x.serialize(stream))
  }

}

object Gesture {
  def computeBoundingBox(strokes: List[GestureStroke]): Rectangle = {
    def loop(strokes: List[GestureStroke], box: Rectangle): Rectangle =
      strokes match {
        case s1 :: ss => loop(ss, box.union(s1.boundingBox))
        case _ => box
      }
    loop(strokes, new Rectangle(0,0))
  }

  def deserialize(stream: DataInput): Gesture =
    new Gesture(stream.readLong(), (for(_ <- 1 to stream.readInt())
      yield GestureStroke.deserialize(stream)).to[List])
}
