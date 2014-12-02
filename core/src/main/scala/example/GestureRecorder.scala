package xyz.seto.obscene.example

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.g2d.Batch
import com.badlogic.gdx.graphics.glutils.ShapeRenderer
import com.badlogic.gdx.graphics.glutils.ShapeRenderer.ShapeType
import com.badlogic.gdx.scenes.scene2d.Actor

import xyz.seto.obscene.Gesture
import xyz.seto.obscene.GestureLibrary;
import xyz.seto.obscene.GestureLibraries;
import xyz.seto.obscene.libgdx.GestureDetector
import xyz.seto.obscene.libgdx.GestureListener


class GestureRecorder(width: Float, height: Float, val name: String) extends Actor with GestureListener {
  val renderer = new ShapeRenderer
  val gDetector = new GestureDetector(this)
  val handle = Gdx.files.external("GestureFile")
  val library = GestureLibraries.fromFile(handle.file)

  Gdx.app.log("Recorder Made", "click");

  library.load()

  // setFillParent(true)
  addListener(gDetector)
  setBounds(0, 0, width, height)

  override def draw(batch: Batch, parentAlpha: Float): Unit = {
    batch.end()

    renderer.setProjectionMatrix(batch.getProjectionMatrix());
    renderer.setTransformMatrix(batch.getTransformMatrix());
    renderer.translate(getX(), getY(), 0);
    renderer.begin(ShapeType.Line)
    renderer.setColor(Color.BLUE)
    gDetector.drawPoints(renderer)
    renderer.end()

    batch.begin()
  }

  def onGesturePreformed(detector: GestureDetector, gesture: Gesture):Boolean = {
    Gdx.app.log("Made a Gesture", "Gest")
    library.addGesture(name, gesture)
    library.save()

    getStage().addActor(new GestureTableView)
    remove()
    true
  }

}
