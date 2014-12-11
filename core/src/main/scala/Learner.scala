package xyz.seto.obscene


object Learner {
  var mInstances = List[Instance]()
}

abstract class Learner {

    def addInstance(instance: Instance) =
        Learner.mInstances = Learner.mInstances :+ instance

    def getInstances() = Learner.mInstances

    def removeInstance(id: Long) =
      Learner.mInstances = Learner.mInstances.filter(i => i.id != id)

    def removeInstances(name: String) =
      Learner.mInstances =
        Learner.mInstances.filter(i => i.label == name)

    def classify(sequenceType: Int, orientationType: Int, instance: Instance): List[Prediction] =
      classify(sequenceType, orientationType, instance.vector)

    def classify(sequenceType: Int, orientationType: Int, vector: List[Float]): List[Prediction]
}
