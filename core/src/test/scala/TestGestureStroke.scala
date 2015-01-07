import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalamock.scalatest.MockFactory

import java.io.DataOutput
import java.io.DataInput
import java.io.IOException

import xyz.seto.obscene.GesturePoint
import xyz.seto.obscene.GestureStroke


class TestGestureStroke extends FlatSpec with MockFactory with Matchers{

  "A datastream" should "deserialize properly" in {
    val input = mock[DataInput]

    inSequence {
      (input.readInt _) expects() returning(1)
      (input.readFloat _) expects() returning(10)
      (input.readFloat _) expects() returning(20)
      (input.readLong _) expects() returning(30)
    }

    val stroke = GestureStroke.deserialize(input)
    val point = stroke.points(0)

    point.x should equal (10f)
    point.y should equal (20f)
    point.timeStamp should equal (30)
    stroke.points should have length 1
  }

  "A Stroke" should "have a proper length" in {
    val stroke = new GestureStroke(List(
      new GesturePoint(0, 0, 10),
      new GesturePoint(10, 10, 20)
    ))

    stroke.points should have length 2
    stroke.length shouldBe (14.142136f)
  }

  "A Stroke" should "error when given malformed data" in {
    val input = mock[DataInput]

    inSequence {
      (input.readInt _) expects() returning(2)
      (input.readFloat _) expects() returning(10)
      (input.readFloat _) expects() returning(20)
      (input.readLong _) expects() returning(30)
      (input.readFloat _) expects() throws new IOException("End of DataInput")
    }

    an [IOException] should be thrownBy GestureStroke.deserialize(input)
  }

  "A Stroke" should "properly flatten points" in {
    val stroke = new GestureStroke(List(
      new GesturePoint(0, 1, 10),
      new GesturePoint(10, 12, 20)
    ))

    val flattened = stroke.flatPoints

    flattened should have length 4
    stroke.points should have length 2
    flattened shouldBe List(0f, 1f, 10f, 12f)
  }

}
