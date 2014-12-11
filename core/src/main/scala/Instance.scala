package xyz.seto.obscene


/**
 * An instance represents a sample if the label is available or a query if the
 * label is null.
 */
object Instance {
  val PATCH_SAMPLE_SIZE = 16;
  val SEQUENCE_SAMPLE_SIZE = 16;

  val ORIENTATIONS = (
    (0.toFloat to math.Pi.toFloat by (math.Pi/4).toFloat) ++
    (0.toFloat to -math.Pi.toFloat by -(math.Pi/4).toFloat)
  ).toList

  def createInstance(sequenceType: Int, orientationType: Int, gesture: Gesture, label: String): Instance  = {
    if (sequenceType == GestureStore.SEQUENCE_SENSITIVE)
      new Instance(
        gesture.id,
        temporalSampler(orientationType, gesture),
        label
      ).normalize
    else
      new Instance(gesture.id, spatialSampler(gesture), label);
  }

  def temporalSampler(orientationType: Int, gesture: Gesture): List[Float] = {
    def orientation(pts: Array[Float], center: Array[Float]): Float = math.atan2(pts(1) - center(1), pts(0) - center(0)).toFloat

    def makeAdjustment(orientation: Float): Float = {
      def loop(adj: Float, orients: List[Float]): Float = orients match {
        case o :: os =>
          if (math.abs(o - orientation) < math.abs(adj))
            loop(math.abs(o - adj), os)
          else
            loop(adj, os)
        case _ => adj
      }

      if (orientationType != GestureStore.ORIENTATION_INVARIANT)
        loop(-orientation, ORIENTATIONS)
      else
        -orientation
    }

    val pts = GestureUtils.temporalSampling(gesture.strokes(0), SEQUENCE_SAMPLE_SIZE)

    val center = GestureUtils.computeCentroid(pts)

    GestureUtils.rotate(
      GestureUtils.translate(pts, -center(0), -center(1)),
      makeAdjustment(orientation(pts, center))
    ).toList
  }

  def spatialSampler(gesture: Gesture): List[Float] =
    GestureUtils.spatialSampling(gesture, PATCH_SAMPLE_SIZE, false).toList
}


class Instance(val id: Long, val vector: List[Float], val label: String) {
  def normalize: Instance = {
    def norm(pts: List[Float], sum: Float): List[Float] = {
      pts map {_ / sum}
    }

    new Instance(id, norm(vector, magnitude), label)
  }

  def magnitude: Float = {
    def sum(pts: List[Float], acu: Float): Float = pts match {
      case p :: ps => sum(ps, acu + (p * p))
      case _ => acu
    }

    math.sqrt(sum(vector, 0)).toFloat
  }
}
