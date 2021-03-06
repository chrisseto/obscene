package xyz.seto.obscene.example

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.Game
import com.badlogic.gdx.Screen
import com.badlogic.gdx.graphics.GL20;
import com.badlogic.gdx.scenes.scene2d.Actor
import com.badlogic.gdx.scenes.scene2d.Stage
import com.badlogic.gdx.utils.viewport.ScreenViewport


class ObsceneExample extends Game {
    override def create() {
        this.setScreen(new ExampleScreen(this))
    }
}


class ExampleScreen(game: Game) extends Screen {
  var stage = new Stage(new ScreenViewport())

  Gdx.input.setInputProcessor(stage);

  stage.addActor(new GestureTableView)
  // stage.addActor(new GestureRecorder(stage.getWidth(), stage.getHeight()))

  def render(delta: Float): Unit = {
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT);

    stage.act(delta);
    stage.draw();

  }

  def resize(width: Int, height: Int): Unit = stage.getViewport().update(width, height, true)
  def dispose(): Unit = stage.dispose()
  def resume(): Unit = {}
  def pause(): Unit = {}
  def hide(): Unit = {}
  def show(): Unit = {}
}
