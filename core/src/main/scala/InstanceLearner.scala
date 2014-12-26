package xyz.seto.obscene

import scala.collection.immutable.TreeMap


class InstanceLearner extends Learner {
  override def classify(sequenceType: Int, orientationType: Int, vector: List[Float]): List[Prediction] = {
    def getDistance(sample: Instance) =
      if (sequenceType == GestureStore.SEQUENCE_SENSITIVE)
        GestureUtils.minimumCosineDistance(sample.vector, vector, orientationType)
      else
        GestureUtils.squaredEuclideanDistance(sample.vector, vector)

    def getWeight(distance: Double): Double =
      if (distance == 0)
        Double.MaxValue
      else
        1 / distance

    def getScore = getWeight _ compose getDistance _

    def updateMap(label: String, score: Double, map: TreeMap[String, Double]): TreeMap[String, Double] =
      map.get(label) match {
        case Some(existing) if (existing == null | existing < score) => map + (label -> score)
        case None => map
        case _ => map
    }

    def loop(instances: List[Instance], predictions: TreeMap[String, Double]): TreeMap[String, Double] = instances match {
      case sample :: is =>
        if (sample.vector.length != vector.length)
          loop(is, predictions)
        else
          loop(is, updateMap(sample.label, getScore(sample), predictions))
      case _ => predictions
    }

    (for ((name: String, score: Double) <- loop(getInstances(), new TreeMap()))
      yield new Prediction(name, score)
      ).toList.sorted
  }
}
