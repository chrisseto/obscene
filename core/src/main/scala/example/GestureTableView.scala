package xyz.seto.obscene.example

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.files.FileHandle
import com.badlogic.gdx.scenes.scene2d.InputEvent
import com.badlogic.gdx.scenes.scene2d.ui.Skin
import com.badlogic.gdx.scenes.scene2d.ui.Label
import com.badlogic.gdx.scenes.scene2d.ui.Table
import com.badlogic.gdx.scenes.scene2d.ui.TextField
import com.badlogic.gdx.scenes.scene2d.ui.TextButton
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener

import java.io.DataOutputStream;

import xyz.seto.obscene.GestureLibrary
import xyz.seto.obscene.GestureLibraries
import xyz.seto.obscene.libgdx.FileHandleGestureLibrary


class GestureTableView extends Table {

  debug()
  setFillParent(true)

  Gdx.app.log("Gesture", "Loading");

  val skin = new Skin(Gdx.files.internal("data/uiskin.json"))
  val library = new FileHandleGestureLibrary(Gdx.files.external("GestureFile"))

  library.load()

  val recordName = new TextField("", skin)
  val recordButton = new TextButton("Record", skin)
  val recognizeButton = new TextButton("Detect", skin)

  recordButton.addListener(new ClickListener() {
    override def clicked(event: InputEvent, x: Float, y: Float) = {
      getStage().addActor(new GestureRecorder(getWidth(), getHeight(), recordName.getText()))
      remove()
    }
  })

  recognizeButton.addListener(new ClickListener() {
    override def clicked(event: InputEvent, x: Float, y: Float) = {
      getStage().addActor(new GestureRecognizer(getWidth(), getHeight()))
      remove()
    }
  })

  add(recognizeButton)

  row()
  row()

  add(new Label("Loaded Gestures:", skin))

  for(gest <- library.getGestureEntries()) {
    row()
    add(new Label(gest, skin))
  }

  row()

  add(recordName)
  add(recordButton)
}

