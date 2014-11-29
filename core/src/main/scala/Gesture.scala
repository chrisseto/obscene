package xyz.seto.obscene

import xyz.seto.obscene.utils.Rectangle
import scala.collection.JavaConversions._
import java.util.ArrayList
import java.io.DataOutputStream;
import java.io.DataInputStream;

class Gesture(var id: Long, var strokes: List[GestureStroke], var boundingBox: Rectangle){
  def this() = this(0, List(), new Rectangle(0, 0))
  def this(stream: DataInputStream) = this(0, List(), new Rectangle(0, 0))

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
