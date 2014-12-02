package xyz.seto.obscene.libgdx;

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.files.FileHandle

import xyz.seto.obscene.GestureLibrary;

class FileHandleGestureLibrary(val handle: FileHandle) extends GestureLibrary {


    override def isReadOnly(): Boolean = true

    def save(): Boolean = {
        if (!mStore.hasChanged()) return true

        mStore.save(handle.write(false), true);
        true;
    }

    def load(): Boolean = {
        Gdx.app.log("Loading", "gesture");
        mStore.load(handle.read(), true);
        true;
    }
}
