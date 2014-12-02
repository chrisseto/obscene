package xyz.seto.obscene;

class Prediction(val name: String, val score: Double) {

  def >(that: Prediction): Boolean = that.score > this.score

  def <(that: Prediction): Boolean = !(that > this)

  override def toString(): String = s"<Prediction $name ($score)>"
}
