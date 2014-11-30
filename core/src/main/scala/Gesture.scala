package xyz.seto.obscene

import xyz.seto.obscene.utils.Rectangle
import scala.collection.JavaConversions._
import java.util.ArrayList
import java.io.DataOutputStream;
import java.io.DataInputStream;

class Gesture(var id: Long, var strokes: List[GestureStroke], var boundingBox: Rectangle){
  def this() = this(0, List(), new Rectangle(0, 0))
  def this(id: Long, strokes: List[GestureStroke]) = this(id, strokes, Gesture.computeBoundingBox(strokes))

  //For Java
  def getID = id
  def getStrokes = new ArrayList(strokes.toBuffer)
  def getBoundingBox(): Rectangle = boundingBox

  def addStroke(stroke: GestureStroke): Unit = {
    boundingBox = boundingBox.union(stroke.boundingBox)
    strokes = strokes :+ stroke
  }

  def serialize(stream: DataOutputStream): Unit = {
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

  def deserialize(stream: DataInputStream): Gesture =
    new Gesture(stream.readLong(), (for(_ <- 1 to stream.readInt())
      yield GestureStroke.deserialize(stream)).to[List])
}
