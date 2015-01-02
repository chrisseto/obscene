package xyz.seto.obscene;

class Prediction(val name: String, val score: Double)  extends Ordered[Prediction]{

  override def >(that: Prediction): Boolean = that.score > this.score

  override def <(that: Prediction): Boolean = that.score < this.score

  def ~=(that: Prediction): Boolean = that.score == this.score

  override def compare(that: Prediction): Int = {
    if(this > that) 1
    else if(this ~= that) 0
    else -1
  }

  override def toString(): String = s"<Prediction $name ($score)>"
}
