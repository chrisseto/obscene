package xyz.seto.obscene;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
// import java.io.IOException;
import java.io.DataOutputStream;
import java.io.DataInputStream;
import java.io.InputStream;
import java.io.OutputStream;
// import java.util.ArrayList;
// import java.util.HashMap;
// import java.util.Set;
// import java.util.Map;
// import java.util.Arrays;

import scala.collection.immutable.HashMap

/**
 * GestureLibrary maintains gesture examples and makes predictions on a new
 * gesture
 */
//
//    File format for GestureStore:
//
//                Nb. bytes   Java type   Description
//                -----------------------------------
//    Header
//                2 bytes     short       File format version number
//                4 bytes     int         Number of entries
//    Entry
//                X bytes     UTF String  Entry name
//                4 bytes     int         Number of gestures
//    Gesture
//                8 bytes     long        Gesture ID
//                4 bytes     int         Number of strokes
//    Stroke
//                4 bytes     int         Number of points
//    Point
//                4 bytes     float       X coordinate of the point
//                4 bytes     float       Y coordinate of the point
//                8 bytes     long        Time stamp
//

object GestureStore {
    val SEQUENCE_INVARIANT = 1;
    // when SEQUENCE_SENSITIVE is used, only single stroke gestures are currently allowed
    val SEQUENCE_SENSITIVE = 2;

    // ORIENTATION_SENSITIVE and ORIENTATION_INVARIANT are only for SEQUENCE_SENSITIVE gestures
    val ORIENTATION_INVARIANT = 1;
    // at most 2 directions can be recognized
    val ORIENTATION_SENSITIVE = 2;
    // at most 4 directions can be recognized
    val ORIENTATION_SENSITIVE_4 = 4;
    // at most 8 directions can be recognized
    val ORIENTATION_SENSITIVE_8 = 8;

    val FILE_FORMAT_VERSION: Short = 1;

    val PROFILE_LOADING_SAVING = false;
}


class GestureStore {
    private var mSequenceType = GestureStore.SEQUENCE_SENSITIVE
    private var mOrientationStyle = GestureStore.ORIENTATION_SENSITIVE

    private var mChanged = false
    private val mClassifier = new InstanceLearner()
    private var mNamedGestures = new HashMap[String, List[Gesture]]();

    def getLearner = mClassifier

    def setOrientationStyle(style: Int) =
      mOrientationStyle = style

    def getOrientationStyle = mOrientationStyle;

    def setSequenceType(sType: Int) =
        mSequenceType = sType

    def getSequenceType = mSequenceType

    def getGestureEntries = mNamedGestures.keySet

    def recognize(gesture: Gesture): List[Prediction] =
      mClassifier.classify(
        mSequenceType,
        mOrientationStyle,
        Instance.createInstance(mSequenceType, mOrientationStyle, gesture, null)
      )

    def addGesture(entryName: String, gesture: Gesture) = {
      if (entryName != null && entryName.length != 0) {
        mNamedGestures.get(entryName) match {
          case Some(gestures) =>
            mNamedGestures += (entryName -> (gesture :: gestures))
          case None =>
            mNamedGestures += (entryName -> List(gesture))
        }
        mClassifier.addInstance(
          Instance.createInstance(mSequenceType, mOrientationStyle, gesture, entryName))
        mChanged = true
      }
    }

    def removeGesture(entryName: String, gesture: Gesture) = {
        mNamedGestures.get(entryName) match {
          case Some(gestures) =>
            gestures.filter(g => g.id != gesture.id) match {
              case gs => mNamedGestures += (entryName -> gs)
              case _ => mNamedGestures -= entryName
            }
          case None =>
        }

        mClassifier.removeInstance(gesture.id)
        mChanged = true
    }

    def removeEntry(entryName: String) = {
        mNamedGestures -= entryName
        mClassifier.removeInstances(entryName)
        mChanged = true
    }

    def getGestures(entryName: String): List[Gesture] =
      mNamedGestures.get(entryName) match {
        case Some(gestures) => gestures
        case _ => null //Empty List?
    }

    def hasChanged = mChanged

    def save(stream: OutputStream): Unit = save(stream, false)

    def save(stream: OutputStream, closeStream: Boolean) = {
      def toDataStream(s: OutputStream): DataOutputStream = {
        def bufferify(is: OutputStream): BufferedOutputStream = s match {
          case st: BufferedOutputStream => st
          case _ => new BufferedOutputStream(s, GestureConstants.IO_BUFFER_SIZE)
        }

        new DataOutputStream(bufferify(s))
      }

      val out = toDataStream(stream)

      def dumpGestures(name: String, gestures: List[Gesture]) = {
        out.writeUTF(name)
        out.writeInt(gestures.size)
        gestures foreach(g => g.serialize(out))
      }

      try {
        out.writeShort(GestureStore.FILE_FORMAT_VERSION)

        // Write number of entries
        out.writeInt(mNamedGestures.size)

        mNamedGestures map (dumpGestures _).tupled

        out.flush()

        mChanged = false;
      } finally {
        if (closeStream)
          out.close()
      }
    }

    /**
     * Load the gesture library
     */
    def load(stream: InputStream): Unit = load(stream, false)

    def load(stream: InputStream, closeStream: Boolean) = {
      def beginLoad(dis: DataInputStream) = dis.readShort() match {
        case 1 => mNamedGestures = readFormatV1(dis)
        case _ => mNamedGestures = new HashMap[String, List[Gesture]]
      }

      def toDataStream(s: InputStream): DataInputStream = {
        def bufferify(is: InputStream): BufferedInputStream = s match {
          case st: BufferedInputStream => st
          case _ => new BufferedInputStream(s, GestureConstants.IO_BUFFER_SIZE)
        }

        new DataInputStream(bufferify(s))
      }

      val in = toDataStream(stream)

      try
        beginLoad(in)
      finally
        if (closeStream)
          in.close()
    }

    private def readFormatV1(in: DataInputStream):HashMap[String, List[Gesture]] = {
      def loadGesture(label: String): Gesture = {
        val gest = Gesture.deserialize(in)
        mClassifier.addInstance(
          Instance.createInstance(mSequenceType, mOrientationStyle, gest, label))
        gest
      }

      def loadGestures(): (String, List[Gesture]) = {
        val name = in.readUTF()

        (name, (for (_ <- 1 to in.readInt())
          yield loadGesture(name)
        ).toList)
      }

      def loop(index: Int, map: HashMap[String, List[Gesture]]): HashMap[String, List[Gesture]] = index match {
        case 0 => map
        case _ => loop(index - 1, map.updated _ tupled loadGestures())
      }

      loop(in.readInt(), new HashMap[String, List[Gesture]])
    }

}
