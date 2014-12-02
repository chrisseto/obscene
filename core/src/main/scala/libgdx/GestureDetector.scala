package xyz.seto.obscene.libgdx;

import scala.collection.mutable.ListBuffer

import java.io.DataOutputStream;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.InputAdapter
import com.badlogic.gdx.utils.TimeUtils
import com.badlogic.gdx.scenes.scene2d.InputEvent
import com.badlogic.gdx.scenes.scene2d.InputListener
import com.badlogic.gdx.graphics.glutils.ShapeRenderer

import xyz.seto.obscene._


class GestureDetector(val listener: GestureListener) extends InputListener {

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

  override def touchDown(event: InputEvent, screenX: Float, screenY: Float, pointer: Int, button: Int) = {
    detectingGesture = true
    pointBuffer = new ListBuffer[GesturePoint]
    pointBuffer += new GesturePoint(screenX, screenY, TimeUtils.millis())
    true
  }

  override def touchDragged(event: InputEvent, screenX: Float, screenY: Float, pointer: Int) = {
    pointBuffer += new GesturePoint(screenX, screenY, TimeUtils.millis())
    true
  }

  override def touchUp(event: InputEvent, screenX: Float, screenY: Float, pointer: Int, button: Int) = {
    detectingGesture = false
    pointBuffer += new GesturePoint(screenX, screenY, TimeUtils.millis())
    jest = new Gesture
    jest.addStroke(new GestureStroke(pointBuffer.to[List]))
    listener.onGesturePreformed(this, jest)
  }

}
