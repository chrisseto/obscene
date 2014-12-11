package xyz.seto.obscene.example

import scala.collection.JavaConversions._

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.g2d.Batch
import com.badlogic.gdx.graphics.glutils.ShapeRenderer
import com.badlogic.gdx.graphics.glutils.ShapeRenderer.ShapeType
import com.badlogic.gdx.scenes.scene2d.Actor

import xyz.seto.obscene.Gesture
import xyz.seto.obscene.Prediction
import xyz.seto.obscene.GestureLibrary
import xyz.seto.obscene.libgdx.GestureDetector
import xyz.seto.obscene.libgdx.GestureListener
import xyz.seto.obscene.libgdx.FileHandleGestureLibrary


class GestureRecognizer(width: Float, height: Float) extends Actor with GestureListener {
  val renderer = new ShapeRenderer
  val gDetector = new GestureDetector(this)
  val handle = Gdx.files.external("GestureFile")
  val library = new FileHandleGestureLibrary(handle)

  Gdx.app.log("Recorder Made", "click");

  library.load()
  addListener(gDetector)
  setBounds(0, 0, width, height)

  override def draw(batch: Batch, parentAlpha: Float): Unit = {
    batch.end()

    renderer.setProjectionMatrix(batch.getProjectionMatrix());
    renderer.setTransformMatrix(batch.getTransformMatrix());
    renderer.translate(getX(), getY(), 0);
    renderer.begin(ShapeType.Line)
    renderer.setColor(Color.YELLOW)
    Gdx.gl20.glLineWidth(20)
    gDetector.drawPoints(renderer)
    renderer.end()

    batch.begin()
  }

  def onGesturePreformed(detector: GestureDetector, gesture: Gesture):Boolean = {
    Gdx.app.log("Detector", "Gesture Detected")
    val pre = library.recognize(gesture).get(0)


    // Gdx.app.log("Detector", "Score of " + pre.score.toString)
    if (pre.score > 2.0)
      Gdx.app.log("Detector", pre.toString())

    true
  }

}
