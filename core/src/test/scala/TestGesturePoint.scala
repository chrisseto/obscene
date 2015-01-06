import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalamock.scalatest.MockFactory

import java.io.DataOutput
import java.io.DataInput

import xyz.seto.obscene.GesturePoint


class TestGesturePoint extends FlatSpec with MockFactory with Matchers{

  "A datastream" should "deserialize properly" in {
    var input = mock[DataInput]

    inSequence {
      (input.readFloat _) expects() returning(10)
      (input.readFloat _) expects() returning(20)
      (input.readLong _) expects() returning(30)
    }

    val point = GesturePoint.deserialize(input)

    point.x should equal (10f)
    point.y should equal (20f)
    point.timeStamp should equal (30)
  }

  "A Gesture Point" should "serialize properly" in {
    var output = mock[DataOutput]
    var point = new GesturePoint(10, 20, 30)

    inSequence {
      (output.writeFloat _) expects (10)
      (output.writeFloat _) expects (20)
      (output.writeLong _) expects (30)
    }

    point.serialize(output)
  }
}
