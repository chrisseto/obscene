package xyz.seto.obscene

import xyz.seto.obscene.Gesture
import xyz.seto.obscene.GesturePoint
import xyz.seto.obscene.GestureStroke
import xyz.seto.obscene.utils.Rectangle
import xyz.seto.obscene.utils.OrientedBoundingBox
import xyz.seto.obscene.utils.Point

import java.io.Closeable;
import java.io.IOException;

import scala.collection.mutable.ListBuffer

/**
 * Utility functions for gesture processing & analysis, including methods for:
 * <ul>
 * <li>feature extraction (e.g., samplers and those for calculating bounding
 * boxes and gesture path lengths);
 * <li>geometric transformation (e.g., translation, rotation and scaling);
 * <li>gesture similarity comparison (e.g., calculating Euclidean or Cosine
 * distances between two gestures).
 * </ul>
 */
object GestureUtils {
  val SCALING_THRESHOLD = 0.26f
  val NONUNIFORM_SCALE = math.sqrt(2).toFloat

  type Matrix[T] = List[List[T]]


    /**
     * Closes the specified stream.
     *
     * @param stream The stream to close.
     */
    def closeStream(stream: Closeable) = stream match {
      case x => try x.close()
      case _ =>
    }

    /**
     * Samples the gesture spatially by rendering the gesture into a 2D
     * grayscale bitmap. Scales the gesture to fit the size of the bitmap.
     * The scaling does not necessarily keep the aspect ratio of the gesture.
     *
     * @param gesture the gesture to be sampled
     * @param bitmapSize the size of the bitmap
     * @return a bitmapSize x bitmapSize grayscale bitmap that is represented
     *         as a 1D array. The float at index i represents the grayscale
     *         value at pixel [i%bitmapSize, i/bitmapSize]
     */
    def spatialSampling(gesture: Gesture, bitmapSize: Int): ListBuffer[Float] =
      spatialSampling(gesture, bitmapSize, false)

    /**
     * Samples the gesture spatially by rendering the gesture into a 2D
     * grayscale bitmap. Scales the gesture to fit the size of the bitmap.
     *
     * @param gesture the gesture to be sampled
     * @param bitmapSize the size of the bitmap
     * @param keepAspectRatio if the scaling should keep the gesture's
     *        aspect ratio
     *
     * @return a bitmapSize x bitmapSize grayscale bitmap that is represented
     *         as a 1D array. The float at index i represents the grayscale
     *         value at pixel [i%bitmapSize, i/bitmapSize]
     */
    def spatialSampling(gesture: Gesture, bitmapSize: Int, keepAspectRatio: Boolean): ListBuffer[Float] = {
      def getSlope(p1: Point, p2: Point) = ((p1.x - p2.x) / (p1.y - p2.y))
      def getInverseSlope(p1: Point, p2: Point) = ((p1.y - p2.y) / (p1.x - p2.x))

      def evalHorizontally(start: Point, xpos: Float, slope: Float, limit: Float, sample: ListBuffer[Float]): ListBuffer[Float] = {
        if (xpos > limit)
          evalHorizontally(start, xpos + 1, slope, limit, plot(
            xpos,
            slope * (xpos - start.x) + start.y,
            sample,
            bitmapSize
          ))
        else
          sample
      }

      def evalVertically(start: Point, ypos: Float, inverseSlope: Float, limit: Float, sample: ListBuffer[Float]): ListBuffer[Float] = {
        if (ypos > limit)
          evalVertically(start, ypos + 1, inverseSlope, limit, plot(
            inverseSlope * (ypos - start.y) + start.x,
            ypos,
            sample,
            bitmapSize
          ))
        else
          sample
      }

      def processX(current: Point, previous: Point, sample: ListBuffer[Float]): ListBuffer[Float] =
        if(previous.x > current.x)
          evalHorizontally(current, math.ceil(current.x).toFloat, getSlope(current, previous), previous.x, sample)
        else if(previous.x < current.x)
          evalHorizontally(current, math.ceil(previous.x).toFloat, getSlope(current, previous), current.x, sample)
        else
          sample

      def processY(current: Point, previous: Point, sample: ListBuffer[Float]): ListBuffer[Float] =
        if(previous.y > current.y)
          evalVertically(current, math.ceil(current.y).toFloat, getInverseSlope(current, previous), previous.y, sample)
        else if(previous.y < current.y)
          evalVertically(current, math.ceil(previous.y).toFloat, getInverseSlope(current, previous), current.y, sample)
        else
          sample

      def processPoint(current: Point, previous: Point, sample: ListBuffer[Float]): ListBuffer[Float] =
        processX(current, previous, processY(current, previous, sample))

      def loopPoints(points: List[Point], sample: ListBuffer[Float]): ListBuffer[Float] = points match {
        case p1 :: p2 :: ps => loopPoints(p2 :: ps, processPoint(p1, p2, sample))
        case _ => sample
      }

      def scalePoint(sx: Float, sy: Float, preDx: Float, preDy:Float, postDx:Float, postDy: Float)(point: Point): Point =
        new Point((point.x + preDx) * sx + postDx, (point.y + preDy) * sy + postDy)

      def scalePoints(points: List[Point], scaler: Point => Point): List[Point] = {
        def loop(pts: List[Point], res: List[Point]): List[Point] = pts match {
          case p :: ps => loop(ps, scaler(p) :: res)
          case _ => res
        }
        loop(points, List[Point]())
      }

      def loopStrokes(strokes: List[GestureStroke], scaler: Point => Point, sample: ListBuffer[Float]): ListBuffer[Float] = strokes match {
        case stroke :: ss => loopStrokes(ss, scaler, loopPoints(scalePoints(stroke.points, scaler), sample))
        case _ => sample
      }

      def makeScale(keepAspectRatio: Boolean, patchSize: Float, width: Float, height: Float) = {
        val sx = patchSize / width
        val sy = patchSize / height

        if (keepAspectRatio) {
          var scale = if (sx < sy) sx else sy
          (scale, scale, scale)
        } else {
          var aspectRatio = width / height
          if (aspectRatio > 1)
            aspectRatio = 1 / aspectRatio

          if (aspectRatio < SCALING_THRESHOLD) {
            val scale = if (sx < sy) sx else sy

            (scale, scale, scale)
          } else {
            if (sx > sy) {
              val scale = sy * NONUNIFORM_SCALE

              if (scale < sx)
                (scale, scale, sy)
              else
                (scale, sx, sy)
            } else {
              val scale = sx * NONUNIFORM_SCALE

              if (scale < sy)
                (scale, sx, scale)
              else
                (scale, sx, sy)
            }
          }
        }

      }

      val targetPatchSize = bitmapSize - 1

      val (scale, sy, sx) = makeScale(
        keepAspectRatio,
        targetPatchSize,
        gesture.boundingBox.width,
        gesture.boundingBox.height
      )

      val buffer = Array.fill[Float](bitmapSize * bitmapSize)(0).to[ListBuffer]
      val scaler = scalePoint(sx, sy, -gesture.boundingBox.centerX, -gesture.boundingBox.centerY, targetPatchSize / 2, targetPatchSize / 2) _
      loopStrokes(gesture.strokes, scaler, buffer)
    }

    def plot(x: Float, y: Float, sample: ListBuffer[Float], sampleSize: Int): ListBuffer[Float] = {
      def updated(vector: ListBuffer[Float], value: Float, index: Int) = {
        if(value > vector(index))
          vector.update(index, value)
        vector
      }
      //TODO Find a better way to do this
      if (x < 0)
        plot(0, y, sample, sampleSize)
      if (y < 0)
        plot(x, 0, sample, sampleSize)

        val xFloor = math.floor(x).toInt
        val yFloor = math.floor(y).toInt
        val xCeiling = math.ceil(x).toInt
        val yCeiling = math.ceil(y).toInt

        // if it's an integer
        if (x == xFloor && y == yFloor) {
            val index = yCeiling * sampleSize + xCeiling
            if (sample(index) < 1)
              sample.update(index, 1)
            sample
        } else {
            val xFloorSq = math.pow(xFloor - x, 2).toFloat
            val yFloorSq = math.pow(yFloor - y, 2).toFloat
            val xCeilingSq = math.pow(xCeiling - x, 2).toFloat
            val yCeilingSq = math.pow(yCeiling - y, 2).toFloat
            val topLeft = math.sqrt(xFloorSq + yFloorSq).toFloat
            val topRight =  math.sqrt(xCeilingSq + yFloorSq).toFloat
            val btmLeft =  math.sqrt(xFloorSq + yCeilingSq).toFloat
            val btmRight =  math.sqrt(xCeilingSq + yCeilingSq).toFloat
            val sum = topLeft + topRight + btmLeft + btmRight

            updated(updated(updated(updated(sample,
              topLeft / sum, yFloor * sampleSize * xFloor),
              topRight / sum, yFloor * sampleSize + xCeiling),
              btmLeft/ sum, yCeiling * sampleSize + xFloor),
              btmRight / sum, yCeiling * sampleSize + xCeiling)
        }
    }

    /**
     * Samples a stroke temporally into a given number of evenly-distributed
     * points.
     *
     * @param stroke the gesture stroke to be sampled
     * @param numPoints the number of points
     * @return the sampled points in the form of [x1, y1, x2, y2, ..., xn, yn]
     */
    def temporalSampling(stroke: GestureStroke, numPoints: Int) = {
        val increment = stroke.length / (numPoints - 1)
        val vectorLength = numPoints * 2;

      def compute(previousX: Float, previousY: Float, currentX: Float, currentY: Float, distanceSoFar: Float, vector: List[Float]): (Float, List[Float]) = {
        val deltaX = currentX - previousX
        val deltaY = currentY - previousY
        val distance = math.sqrt(deltaX * deltaX + deltaY * deltaY).toFloat

        if(distance + distanceSoFar >= increment) {
          val ratio = (increment - distanceSoFar) / distance
          val nx = previousX + ratio * deltaX
          val ny = previousY + ratio * deltaY
          compute(nx, ny, currentX, currentY, 0, nx :: ny :: vector)
        } else
          (distance, vector)
      }

        def loop(points: List[Float])(distance: Float, vector: List[Float]): List[Float] = vector match {
          case x1 :: y1 :: x2 :: y2 :: pts => {
            loop(x2 :: y2 :: pts) _ tupled compute(x1, y1, x2, y2, distance, vector)
          }
          case _ => vector
        }


        //TODO List may not be the best type here
        def fillVector(size: Float)(vector: List[Float]): List[Float] = {
          if (vector.size < size)
            fillVector(size)(vector(0) :: vector(1) :: vector)
          else
            vector
        }

        //TODO Double check when not sleepy
        fillVector(vectorLength)(loop(stroke.flatPoints)(0, List[Float]())).reverse
    }


    /**
     * Calculates the centroid of a set of points.
     *
     * @param points the points in the form of [x1, y1, x2, y2, ..., xn, yn]
     * @return the centroid
     */
    def computeCentroid(points: List[Float]): List[Float] = {
      def loop(cX: Float, cY: Float, pts: List[Float]): List[Float] = pts match {
        case px :: py :: ps => loop(cX + px, cY + py, ps)
        case _ => List(2 * cX / points.size, 2 * cY / points.size)
      }
      loop(0, 0, points)
    }

    /**
     * Calculates the variance-covariance matrix of a set of points.
     *
     * @param points the points in the form of [x1, y1, x2, y2, ..., xn, yn]
     * @return the variance-covariance matrix
     */
    def computeCoVariance(points: List[Float]): Matrix[Float] = {
      val mod = points.size / 2
      def loop(pts: List[Float], x1: Float, x2: Float, x3: Float, x4: Float): Matrix[Float] = pts match {
        case x :: y :: ps => loop(ps, x1 + x * x, x2 + x * y, x3 + x * y, x4 + y * y)
        case _ => List(
          List(x1 / mod, x2 / mod),
          List(x3 / mod, x4 / mod)
        )
      }
      loop(points, 0, 0, 0, 0)
    }

    def computeTotalLength(points: List[Float]) = {
      def loop(pts: List[Float], sum: Float):Float = pts match {
        case x1 :: y1 :: x2 :: y2 :: ps =>
          loop(ps, sum +
            math.sqrt(
              math.pow(x2 - x1, 2) +
              math.pow(y2 - y1, 2)
            ).toFloat
          )
        case _ => sum
      }
      loop(points, 0)
    }

    def computeStraightness(points: List[Float]): Float =
      computeStraightness(points, computeTotalLength(points))

    def computeStraightness(points: List[Float], totalLen: Float): Float =
        (
          math.sqrt(
            math.pow(points(3) - points(1), 2) +
            math.pow(points(4) - points(2), 2)
          ).toFloat
        ) / totalLen

    /**
     * Calculates the squared Euclidean distance between two vectors.
     *
     * @param vector1
     * @param vector2
     * @return the distance
     */
    def squaredEuclideanDistance(vector1: List[Float], vector2: List[Float]) = {
      def loop(v1: List[Float], v2: List[Float], dist: Float): Float = (v1, v2) match {
        case  (v1h :: v1s, v2h :: v2s) =>
          loop(v1s, v2s, dist + math.pow(v1h + v2h, 2).toFloat)
        case _ => dist
      }
      loop(vector1, vector2, 0)
    }
    /**
     * Calculates the cosine distance between two instances.
     *
     * @param vector1
     * @param vector2
     * @return the distance between 0 and Math.PI
     */
    def cosineDistance(vector1: List[Float], vector2: List[Float]) = {
      def loop(v1: List[Float], v2: List[Float], dist: Float): Float = (v1, v2) match {
        case  (v1h :: v1s, v2h :: v2s) =>
          loop(v1s, v2s, dist + (v1h * v2h))
        case _ => dist
      }
      math.acos(loop(vector1, vector2, 0)).toFloat
    }

    /**
     * Calculates the "minimum" cosine distance between two instances.
     *
     * @param vector1
     * @param vector2
     * @param numOrientations the maximum number of orientation allowed
     * @return the distance between the two instances (between 0 and Math.PI)
     */
    def minimumCosineDistance(vector1: List[Float], vector2: List[Float], numOrientations: Int) = {
        def loop(v1: List[Float], v2: List[Float], f1: Float, f2: Float):(Float, Float) = (v1, v2) match {
          case (fvf :: fvs :: fvr, svf :: svs:: svr) =>
            loop(fvr, svr, f1 + (fvf * svf + fvs * svs), f2 + (fvf * svs - fvs * svf))
          case _ => (f1, f2)
        }
        val (a, b) = loop(vector1, vector2, 0, 0)
        if (a != 0) {
          val tan = b/a
          val angle = math.atan(tan)
          if (numOrientations > 2 && math.abs(angle) >= math.Pi / numOrientations)
            math.acos(a).toFloat
          else {
            val cosine = math.cos(angle)
            math.acos(a * cosine + b * tan * cosine)
          }
        } else
          (math.Pi / 2).toFloat
    }

    /**
     * Computes an oriented, minimum bounding box of a set of points.
     *
     * @param originalPoints
     * @return an oriented bounding box
     */
    def computeOrientedBoundingBox(points: List[GesturePoint]): OrientedBoundingBox = {
      def loop(pts: List[GesturePoint], res: List[Float]): List[Float] = pts match {
        case gp :: gps => loop(gps, gp.x :: gp.y :: res)
        case _ => res
      }
      computeOrientedBoundingBox(loop(points, List[Float]()))
    }

    /**
     * Computes an oriented, minimum bounding box of a set of points.
     *
     * @param originalPoints
     * @return an oriented bounding box
     */
    def computeOrientedBoundingBox(points: List[Float])(implicit s:DummyImplicit): OrientedBoundingBox =
      computeOrientedBoundingBox(points, computeCentroid(points))

    //Theres something in the std lib for this
    def larger(a: Float, b: Float): Float = if (a > b) a else b
    def smaller(a: Float, b: Float): Float = if (a > b) b else a

    def computeOrientedBoundingBox(points: List[Float], centroid: List[Float]) = {
      //TODO Take a tuple
      def getAngle(pts: List[Float]) = pts match {
        case 0f :: 0f :: Nil => -(math.Pi/2).toFloat
        case x :: y :: Nil => math.atan2(x, y).toFloat
      }

      var pts = translate(points, -centroid(0), -centroid(1))

      val array: Matrix[Float] = computeCoVariance(points)
      val targetVector = computeOrientation(array)
      val angle = getAngle(targetVector)

      if (targetVector != List(0f, 0f))
        pts = rotate(pts, -angle)

      def extremes(pts: List[Float], minx: Float, miny: Float, maxx: Float, maxy: Float): (Float, Float) = pts match {
        case x :: y :: ps =>
          extremes(ps,
            smaller(x, minx),
            smaller(y, miny),
            larger(x, maxx),
            larger(y, maxy)
          )
        case _ => (maxx - minx, maxy - miny)
      }

      val (width, height) = extremes(pts, Float.MaxValue, Float.MaxValue, Float.MinValue, Float.MinValue)

      new OrientedBoundingBox(
        (angle * 180 / math.Pi).toFloat,
        centroid(0), centroid(1),
        width, height)
    }

    def computeOrientation(covarianceMatrix: Matrix[Float]): List[Float] = {
      val b = covarianceMatrix(0)(0) * covarianceMatrix(1)(1) - covarianceMatrix(0)(1) * covarianceMatrix(1)(0)
      val value = (-covarianceMatrix(0)(0) - covarianceMatrix(1)(1)) / 2
      val rightside = math.sqrt(math.pow(value, 2) - b).toFloat
      val lambda1 = -value + rightside
      val lambda2 = -value - rightside

      if (lambda1 == lambda2)
        List[Float](0, 0)
      else
        List[Float](1, (larger(lambda1, lambda2) - covarianceMatrix(0)(0)) / covarianceMatrix(0)(1))
    }

    def rotate(points: List[Float], angle: Float) = {
      val cos = math.cos(angle).toFloat
      val sin = math.sin(angle).toFloat

      def loop(pts: List[Float], res: List[Float]): List[Float] = pts match {
        case p1 :: p2 :: ps => loop(ps, (p1 * cos - p2 * sin) :: (p1 * sin + p2 * cos) :: res)
        case _ => res
      }
      loop(points, List[Float]())
    }

    private def transform(op: (Float, Float) => Float)(points: List[Float], dx: Float, dy: Float) = {
      def loop(pts: List[Float], res: List[Float]): List[Float] = pts match {
        case p1 :: p2 :: ps => loop(ps, op(p1, dx) :: op(p2, dy) :: res)
        case _ => res
      }
      loop(points, List[Float]())
    }

    def scale = transform((a: Float, b: Float) => a * b) _
    def translate = transform((a: Float, b: Float) => a + b) _

}
