package xyz.seto.obscene

import java.io.DataOutput
import java.io.DataInput

import xyz.seto.obscene.utils.Rectangle


class Gesture(val id: Long, val strokes: List[GestureStroke], val boundingBox: Rectangle){
  def this(id: Long, strokes: List[GestureStroke]) =
    this(id, strokes, Gesture.computeBoundingBox(strokes))

  def this() = this(0, List())

  /** Add the given stroke to the current gesture
   * @param stroke The stroke to add
   * @return The new Gesture with stroke appended to the list of strokes
   */
  def addStroke(stroke: GestureStroke): Gesture =
    new Gesture(id, strokes :+ stroke, boundingBox.union(stroke.boundingBox))

  def serialize(stream: DataOutput): Unit = {
    stream writeLong id
    stream writeInt strokes.length
    this.strokes.map(stroke => stroke serialize stream)
  }

}

object Gesture {
  /** Generate the bounding box of the entire gesture
   * computed by unioning the boundingboxes of all the contained strokes
   * @param strokes The list of GestureStrokes to aggregate
   * @return The gesture's bounding box
   */
  def computeBoundingBox(strokes: List[GestureStroke]): Rectangle = {
    def loop(strokes: List[GestureStroke], box: Rectangle): Rectangle =
      strokes match {
        case s1 :: ss => loop(ss, box union s1.boundingBox)
        case _ => box
      }
    loop(strokes, new Rectangle(0,0))
  }

  /** Deserialize a gesture from the given data input
   * A gesture is defined
   * as an id (Long)
   * followed by the number of strokes contained (Integer)
   * finally followed by x strokes
   * @return The deserialized gesture
   */
  def deserialize(stream: DataInput): Gesture =
    new Gesture(
      stream.readLong(),
      (for(_ <- 1 to stream.readInt())
        yield GestureStroke deserialize stream
      ).toList
    )
}
