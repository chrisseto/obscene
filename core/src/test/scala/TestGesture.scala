import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalamock.scalatest.MockFactory

import java.io.DataOutput
import java.io.DataInput
import java.io.IOException

import xyz.seto.obscene.Gesture
import xyz.seto.obscene.GesturePoint
import xyz.seto.obscene.GestureStroke


class TestGesture extends FlatSpec with MockFactory with Matchers{

  "A datastream" should "deserialize properly" in {
    val input = mock[DataInput]

    inSequence {
      (input.readLong _) expects() returning(1337)
      (input.readInt _) expects() returning(1)
      (input.readInt _) expects() returning(1)
      (input.readFloat _) expects() returning(10)
      (input.readFloat _) expects() returning(20)
      (input.readLong _) expects() returning(30)
    }

    val gesture = Gesture.deserialize(input)
    val stroke = gesture.strokes(0)
    val point = stroke.points(0)

    point.x should equal (10f)
    point.y should equal (20f)
    point.timeStamp should equal (30)
    stroke.points should have length 1
    gesture.id shouldBe (1337)
    gesture.strokes should have length 1
  }

}
