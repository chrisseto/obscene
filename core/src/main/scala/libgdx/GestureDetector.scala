package xyz.seto.obscene.libgdx

import scala.collection.mutable.ListBuffer

import com.badlogic.gdx.InputAdapter
import com.badlogic.gdx.utils.TimeUtils
import com.badlogic.gdx.graphics.glutils.ShapeRenderer

import xyz.seto.obscene._


class GestureDetector extends InputAdapter {

  var jest: Gesture = new Gesture
  var detectingGesture = false
  var pointBuffer: ListBuffer[GesturePoint] = new ListBuffer[GesturePoint]

  def numPoints: Int = pointBuffer.length

  def drawPoints(sr: ShapeRenderer): Unit = {
    def loop(points: List[GesturePoint]): Unit = points match {
        case p1 :: p2 :: ps => sr.line(p1.x, p1.y, p2.x, p2.y); loop(p2 :: ps)
        case _ =>
    }
    loop(pointBuffer.to[List])
  }

  override def touchDown(screenX: Int, screenY: Int, pointer: Int, button: Int) = {
    detectingGesture = true
    pointBuffer = new ListBuffer[GesturePoint]
    true
  }

  override def touchDragged(screenX: Int, screenY: Int, pointer: Int) = {
    pointBuffer += new GesturePoint(screenX, screenY, TimeUtils.millis())
    true
  }

  override def touchUp(screenX: Int, screenY: Int, pointer: Int, button: Int) = {
    detectingGesture = false
    jest = new Gesture
    jest.addStroke(new GestureStroke(pointBuffer.to[List]))
    true
  }

}
