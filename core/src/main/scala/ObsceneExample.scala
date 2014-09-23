package xyz.seto.obscene.example

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.Game
import com.badlogic.gdx.Screen
import com.badlogic.gdx.graphics.GL20;
import com.badlogic.gdx.graphics.g2d.BitmapFont
import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.glutils.ShapeRenderer
import com.badlogic.gdx.graphics.glutils.ShapeRenderer.ShapeType

import xyz.seto.obscene._
import xyz.seto.obscene.libgdx.GestureDetector

class ObsceneExample extends Game {
    override def create() {
        this.setScreen(new ExampleScreen(this))
    }
}


class ExampleScreen(game: Game) extends Screen {

  val gDetector = new GestureDetector
  val font = new BitmapFont
  val batch = new SpriteBatch
  val shapes = new ShapeRenderer

  Gdx.input.setInputProcessor(gDetector);

  def dispose(): Unit = {}
  def hide(): Unit = {}
  def pause(): Unit = {}

  def render(delta: Float): Unit = {
    Gdx.gl.glClearColor(1, 1, 1, 1)
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT);
    Gdx.gl.glLineWidth(20)

    batch.begin()
    font.draw(batch, gDetector.numPoints.toString, 0, 50)
    batch.end()

    shapes.begin(ShapeType.Line)
    shapes.setColor(0, 0, 0, 1)
    gDetector.drawPoints(shapes)
    shapes.end()
  }

  def resize(x$1: Int,x$2: Int): Unit = {}
  def resume(): Unit = {}
  def show(): Unit = {}
}
