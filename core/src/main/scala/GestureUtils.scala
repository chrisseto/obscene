package xyz.seto.obscene

import xyz.seto.obscene.Gesture
import xyz.seto.obscene.GesturePoint
import xyz.seto.obscene.GestureStroke
import xyz.seto.obscene.utils.Rectangle

import java.util.ArrayList;
import java.util.Arrays;
import java.io.Closeable;
import java.io.IOException;

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
    val NONUNIFORM_SCALE = (float) math.sqrt(2)
}

class GestureUtils {
  
  type Matrix[T] = List[List[T]]


    /**
     * Closes the specified stream.
     *
     * @param stream The stream to close.
     */
    def closeStream(stream: Closeable) = stream match {
      case x => try x.close() catch {}
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
    def spatialSampling(gesture: Gesture, bitmapSize: Int): List[Float] =
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
    def spatialSampling(gesture: Gesture, bitmapSize: Int, keepAspectRatio: Boolean) {
        final float targetPatchSize = bitmapSize - 1;
        float[] sample = new float[bitmapSize * bitmapSize];
        Arrays.fill(sample, 0);

        Rectangle rect = gesture.getBoundingBox();
        final float gestureWidth = rect.width();
        final float gestureHeight = rect.height();
        float sx = targetPatchSize / gestureWidth;
        float sy = targetPatchSize / gestureHeight;

        if (keepAspectRatio) {
            float scale = sx < sy ? sx : sy;
            sx = scale;
            sy = scale;
        } else {

            float aspectRatio = gestureWidth / gestureHeight;
            if (aspectRatio > 1) {
                aspectRatio = 1 / aspectRatio;
            }
            if (aspectRatio < SCALING_THRESHOLD) {
                float scale = sx < sy ? sx : sy;
                sx = scale;
                sy = scale;
            } else {
                if (sx > sy) {
                    float scale = sy * NONUNIFORM_SCALE;
                    if (scale < sx) {
                        sx = scale;
                    }
                } else {
                    float scale = sx * NONUNIFORM_SCALE;
                    if (scale < sy) {
                        sy = scale;
                    }
                }
            }
        }
        float preDx = -rect.centerX();
        float preDy = -rect.centerY();
        float postDx = targetPatchSize / 2;
        float postDy = targetPatchSize / 2;
        final ArrayList<GestureStroke> strokes = gesture.getStrokes();
        final int count = strokes.size();
        int size;
        float xpos;
        float ypos;
        for (int index = 0; index < count; index++) {
            final GestureStroke stroke = strokes.get(index);
            float[] strokepoints = stroke.flatPoints();
            size = strokepoints.length;
            final float[] pts = new float[size];
            for (int i = 0; i < size; i += 2) {
                pts[i] = (strokepoints[i] + preDx) * sx + postDx;
                pts[i + 1] = (strokepoints[i + 1] + preDy) * sy + postDy;
            }
            float segmentEndX = -1;
            float segmentEndY = -1;
            for (int i = 0; i < size; i += 2) {
                float segmentStartX = pts[i] < 0 ? 0 : pts[i];
                float segmentStartY = pts[i + 1] < 0 ? 0 : pts[i + 1];
                if (segmentStartX > targetPatchSize) {
                    segmentStartX = targetPatchSize;
                }
                if (segmentStartY > targetPatchSize) {
                    segmentStartY = targetPatchSize;
                }
                plot(segmentStartX, segmentStartY, sample, bitmapSize);
                if (segmentEndX != -1) {
                    // Evaluate horizontally
                    if (segmentEndX > segmentStartX) {
                        xpos = (float) Math.ceil(segmentStartX);
                        float slope = (segmentEndY - segmentStartY) /
                                      (segmentEndX - segmentStartX);
                        while (xpos < segmentEndX) {
                            ypos = slope * (xpos - segmentStartX) + segmentStartY;
                            plot(xpos, ypos, sample, bitmapSize);
                            xpos++;
                        }
                    } else if (segmentEndX < segmentStartX){
                        xpos = (float) Math.ceil(segmentEndX);
                        float slope = (segmentEndY - segmentStartY) /
                                      (segmentEndX - segmentStartX);
                        while (xpos < segmentStartX) {
                            ypos = slope * (xpos - segmentStartX) + segmentStartY;
                            plot(xpos, ypos, sample, bitmapSize);
                            xpos++;
                        }
                    }
                    // Evaluate vertically
                    if (segmentEndY > segmentStartY) {
                        ypos = (float) Math.ceil(segmentStartY);
                        float invertSlope = (segmentEndX - segmentStartX) /
                                            (segmentEndY - segmentStartY);
                        while (ypos < segmentEndY) {
                            xpos = invertSlope * (ypos - segmentStartY) + segmentStartX;
                            plot(xpos, ypos, sample, bitmapSize);
                            ypos++;
                        }
                    } else if (segmentEndY < segmentStartY) {
                        ypos = (float) Math.ceil(segmentEndY);
                        float invertSlope = (segmentEndX - segmentStartX) /
                                            (segmentEndY - segmentStartY);
                        while (ypos < segmentStartY) {
                            xpos = invertSlope * (ypos - segmentStartY) + segmentStartX;
                            plot(xpos, ypos, sample, bitmapSize);
                            ypos++;
                        }
                    }
                }
                segmentEndX = segmentStartX;
                segmentEndY = segmentStartY;
            }
        }
        return sample;
    }
    public static float[] spatialSampling(Gesture gesture, int bitmapSize,
            boolean keepAspectRatio) {
        final float targetPatchSize = bitmapSize - 1;
        float[] sample = new float[bitmapSize * bitmapSize];
        Arrays.fill(sample, 0);

        Rectangle rect = gesture.getBoundingBox();
        final float gestureWidth = rect.width();
        final float gestureHeight = rect.height();
        float sx = targetPatchSize / gestureWidth;
        float sy = targetPatchSize / gestureHeight;

        if (keepAspectRatio) {
            float scale = sx < sy ? sx : sy;
            sx = scale;
            sy = scale;
        } else {

            float aspectRatio = gestureWidth / gestureHeight;
            if (aspectRatio > 1) {
                aspectRatio = 1 / aspectRatio;
            }
            if (aspectRatio < SCALING_THRESHOLD) {
                float scale = sx < sy ? sx : sy;
                sx = scale;
                sy = scale;
            } else {
                if (sx > sy) {
                    float scale = sy * NONUNIFORM_SCALE;
                    if (scale < sx) {
                        sx = scale;
                    }
                } else {
                    float scale = sx * NONUNIFORM_SCALE;
                    if (scale < sy) {
                        sy = scale;
                    }
                }
            }
        }
        float preDx = -rect.centerX();
        float preDy = -rect.centerY();
        float postDx = targetPatchSize / 2;
        float postDy = targetPatchSize / 2;
        final ArrayList<GestureStroke> strokes = gesture.getStrokes();
        final int count = strokes.size();
        int size;
        float xpos;
        float ypos;
        for (int index = 0; index < count; index++) {
            final GestureStroke stroke = strokes.get(index);
            float[] strokepoints = stroke.flatPoints();
            size = strokepoints.length;
            final float[] pts = new float[size];
            for (int i = 0; i < size; i += 2) {
                pts[i] = (strokepoints[i] + preDx) * sx + postDx;
                pts[i + 1] = (strokepoints[i + 1] + preDy) * sy + postDy;
            }
            float segmentEndX = -1;
            float segmentEndY = -1;
            for (int i = 0; i < size; i += 2) {
                float segmentStartX = pts[i] < 0 ? 0 : pts[i];
                float segmentStartY = pts[i + 1] < 0 ? 0 : pts[i + 1];
                if (segmentStartX > targetPatchSize) {
                    segmentStartX = targetPatchSize;
                }
                if (segmentStartY > targetPatchSize) {
                    segmentStartY = targetPatchSize;
                }
                plot(segmentStartX, segmentStartY, sample, bitmapSize);
                if (segmentEndX != -1) {
                    // Evaluate horizontally
                    if (segmentEndX > segmentStartX) {
                        xpos = (float) Math.ceil(segmentStartX);
                        float slope = (segmentEndY - segmentStartY) /
                                      (segmentEndX - segmentStartX);
                        while (xpos < segmentEndX) {
                            ypos = slope * (xpos - segmentStartX) + segmentStartY;
                            plot(xpos, ypos, sample, bitmapSize);
                            xpos++;
                        }
                    } else if (segmentEndX < segmentStartX){
                        xpos = (float) Math.ceil(segmentEndX);
                        float slope = (segmentEndY - segmentStartY) /
                                      (segmentEndX - segmentStartX);
                        while (xpos < segmentStartX) {
                            ypos = slope * (xpos - segmentStartX) + segmentStartY;
                            plot(xpos, ypos, sample, bitmapSize);
                            xpos++;
                        }
                    }
                    // Evaluate vertically
                    if (segmentEndY > segmentStartY) {
                        ypos = (float) Math.ceil(segmentStartY);
                        float invertSlope = (segmentEndX - segmentStartX) /
                                            (segmentEndY - segmentStartY);
                        while (ypos < segmentEndY) {
                            xpos = invertSlope * (ypos - segmentStartY) + segmentStartX;
                            plot(xpos, ypos, sample, bitmapSize);
                            ypos++;
                        }
                    } else if (segmentEndY < segmentStartY) {
                        ypos = (float) Math.ceil(segmentEndY);
                        float invertSlope = (segmentEndX - segmentStartX) /
                                            (segmentEndY - segmentStartY);
                        while (ypos < segmentStartY) {
                            xpos = invertSlope * (ypos - segmentStartY) + segmentStartX;
                            plot(xpos, ypos, sample, bitmapSize);
                            ypos++;
                        }
                    }
                }
                segmentEndX = segmentStartX;
                segmentEndY = segmentStartY;
            }
        }
        return sample;
    }

    def plot(x: Float, y: Float, sample: List[Float], sampleSize: Int): List[Float] = {
      def update(vector: List[Float], value: Float, index: Int) = {
        if(value > vector(index))
          vector.updated(index, value)
        else
          vector
      }
        x = x < 0 ? 0 : x
        y = y < 0 ? 0 : y

        val xFloor = math.floor(x).toInt
        val yFloor = math.floor(y).toInt
        val xCeiling = math.ceil(x).toInt
        val yCeiling = math.ceil(y).toInt

        // if it's an integer
        if (x == xFloor && y == yFloor) {
            val index = yCeiling * sampleSize + xCeiling
            if (sample(index) < 1){
                sample.update(index, 1)
            }
        } else {
            val xFloorSq = math.pow(xFloor - x, 2);
            val yFloorSq = math.pow(yFloor - y, 2);
            val xCeilingSq = math.pow(xCeiling - x, 2);
            val yCeilingSq = math.pow(yCeiling - y, 2);
            val topLeft = math.sqrt(xFloorSq + yFloorSq)
            val topRight =  math.sqrt(xCeilingSq + yFloorSq)
            val btmLeft =  math.sqrt(xFloorSq + yCeilingSq)
            val btmRight =  math.sqrt(xCeilingSq + yCeilingSq)
            val sum = topLeft + topRight + btmLeft + btmRight

            updated(update(update(updated(sample,
              topLeft / sum, yFloor * sampleSize * xFloor),
              topRight / sum, yFloat * sampleSize + xCeiling),
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

        def buildVector(points: List[Float], previousX: Float, previousY: Float, distanceSoFar: Float, vector: List[Float]): (List[Float], Float, Float) = {
          case currentX :: currentY :: pts => {
            val deltaX = currentX - previousX
            val deltaY = currentY - previousY
            val distance = math.sqrt(deltaX * deltaX + deltaY * deltaY)
            if (distanceSoFar + distance >= increment) {
              val ratio = (increment - distanceSoFar) / distance
              val nx = previousX + ratio * deltaX
              val ny = previousY + ratio * deltaY
              buildVector(pts, currentX, currentY, 0, vector :: nx :: ny)
            } else {
              buildVector(pts, Float.MinValue, Float.MinValue, distanceSoFar, vector)
            }
          }
          case _ => (vector, previousX, previousY)
        }

        def fillVector(vector: List[Float], x: Float, y: Float, size: Float) = {
          if (vector.size < size)
            fillVector(vector :: x :: y, x, y, size)
          else
            vector
        }

        fillVector _ tupled buildVector(stroke.flatPoints, Float.MinValue, Float.MinValue, 0, new List[Float]), vectorLength
    }


    /**
     * Calculates the centroid of a set of points.
     *
     * @param points the points in the form of [x1, y1, x2, y2, ..., xn, yn]
     * @return the centroid
     */
    def computeCentroid(points: List[Float]): List[Float] = {
      def loop(cX: Float, cY: Float, pts: List[Float]) = pts match {
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
    def computeCoVariance(points: List[Float]): Matrix[List] = {
      val mod = points.size / 2
      def loop(pts: List[Float], x1: Float, x2: Float, x3: Float, x4: Float) = pts match {
        case x :: y :: ps => loop(ps, x1 + x * x, x2 + x * y, x3 + x * y, x4 + y * y)
        case _ => new List(
            new List(x1 / mod, x2 / mod),
            new List(x3 / mod, x4 / mod)
          )
      }
      def loop(points, 0, 0, 0, 0)
    }

    def computeTotalLength(points: List[Float]) = {
      def loop(pts: List[Float], sum: Float) = pts match {
        case x1 :: y1 :: x2 :: y2 :: ps => 
          loop(ps, sum +
            math.sqrt(
              math.pow(x2 - x1, 2),
              math.pow(y2 - y1, 2)
            ).toFloat
          )
        case _ => sum
      }
      loop(points, 0)
    }

    def computerStraightness(points: List[Float]) = 
      computeStraightness(points, computeTotalLength(points))

    def computerStraightness(points: List[Float], totalLen: Float) = points match {
      case x1 :: y1 :: x2 :: y2 =>
        (math.sqrt(math.pow(x2 - x1, 2), math.pow(y2 - y1, 2)).toFloat) / totalLen
    }

    /**
     * Calculates the squared Euclidean distance between two vectors.
     *
     * @param vector1
     * @param vector2
     * @return the distance
     */
    def squaredEuclideanDistance(vector1: List[Float], vector2: List[Float]) = {
      def loop(v1: List[Float], v2: List[Float], dist: Float) = (v1, v2) match {
        case  (v1h :: v1s, v2h :: v2s) =>
          loop(v1s, v2s, dist + math.pow(v1h = v2h, 2).toFloat)
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
      def loop(v1: List[Float], v2: List[Float], dist: Float) = (v1, v2) match {
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
        def loop(v1: List[Float], v2: List[Float], f1: Float, f2: Float) = (v1, v2) match {
          case (fvf :: fvs :: fvr, svf :: svs:: svr) =>
            loop(fvr, svr, f1 + (fvf * svf + fvs * svs), f2 + (fvf * svs - fvs * svf))
          case _ => (f1, f2)
        }
        val a, b = loop(vector1, vector2, 0, 0)
        if (a != 0)
          val tan = b/a
          val angle = math.atan(tan)
          if (numOrientations > 2 && math.abs(angle) >= math.PI / numOrientations)
            math.acos(a).toFloat
          else {
            val cosine = math.cos(angle)
            math.acos(a * cosine + b * tan * cosine)
          }
        else
          (math.PI / 2).toFloat
    }

    /**
     * Computes an oriented, minimum bounding box of a set of points.
     *
     * @param originalPoints
     * @return an oriented bounding box
     */
    def computeOrientedBoundingBox(points: List[GesturePoint]) = {
      def loop(pts: List[GesturePoint], res: List[Float]): List[Float] = pts match {
        case gp :: gps => loop(gps, gps :: gp.x :: gp.y)
        case _ => res
      }
      computeOrientedBoundingBox(loop(points, new List[Float]))
    }

    /**
     * Computes an oriented, minimum bounding box of a set of points.
     *
     * @param originalPoints
     * @return an oriented bounding box
     */
    def computeOrientedBoundingBox(points: List[Float]) =
      computeOrientedBoundingBox(points, computeCentroid(points))

    def larger(a: Float, b: Float): Float = if (a > b) a else b
    def smaller(a: Float, b: Float): Float = if (a > b) b else a

    def computeOrientedBoundingBox(points: List[Float], centroid: List[Float]) = {
      def getAngle(pts: List[Float]) = pts match {
        case 0 :: 0 => -(math.PI/2).toFloat
        case x :: y => math.atan2(x, y).toFloat
      }
      points = translate(points, -centroid[0], -centroid[1])
      val array: Matrix[Float] = computeCoVariance(points)
      val targetVector = computeOrientation(array)
      val angle = getAngle(targetVector)
      if (targetVector != 0 :: 0)
        points = rotate(points, -angle)
      def extremes(pts: List[Float], minx: Float, miny: Float, maxx: Float, maxy: Float): (Float, Float) = pts match {
        case x :: y :: ps =>
          loop(ps,
            smaller(x, minx),
            smaller(y, miny),
            larger(x, maxx),
            larger(y, maxy)
          )
        case _ => (maxx - minx, maxy - miny)
      }
      new OrientedBoundingBox(
        (angle * 180 / math.Pi).toFloat,
        centroid[0], centroid[1],
        extremes(points, Float.MaxValue, Float.MaxValue, Float.MinValue, Float.MinValue))
    }

    def computeOrientation(covarianceMatrix: Matrix[Float]): List[Float] = {
      val b = covarianceMatrix[0][0] * covarianceMatrix[1][1] - covarianceMatrix[0][1] * covarianceMatrix[1][0]
      val value = (-covarianceMatrix[0][0] - covarianceMatrix[1][1]) / 2
      val rightside = math.sqrt(math.pow(value, 2) - b).toFloat
      val lambda1 = -value + rightside
      val lambda2 = -value - rightside

      if (lambda1 == lambda2)
        List[Float](0, 0)
      else
        List[Float](1, (larger(lambda1, lambda2) - covarianceMatrix[0][0]) / covarianceMatrix[0][1])
    }

    def rotate(points: List[Float], angle: Float) = {
      val cos = math.cos(angle).toFloat
      val sin = math.sin(angle).toFloat

      def loop(pts: List[Float], res: List[Float]): List[Float] = pts match {
        case p1 :: p2 :: ps => loop(ps, (p1 * cos - p2 * sin) :: (p1 * sin + p2 * cos) :: res)
        case _ => res
      }
      loop(points, new List[Float])
    }

    private def _transform(op: (Float, Float) => Float, points: List[Float], dx: Float, dy: Float) = {
      def loop(pts: List[Float], res: List[Float]): List[Float] = pts match {
        case p1 :: p2 :: ps => loop(ps, op(p1, sx) :: op(p2, sy) :: res)
        case _ => res
      }
      loop(points, new List[Float])
    }

    def scale = _transform ((a: Float, b: Float) => a * b) _
    def translate = _transform ((a: Float, b: Float) => a + b) _

}
