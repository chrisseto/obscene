package xyz.seto.obscene;

class Prediction(val name: String, val score: Double)  extends Ordered[Prediction]{

  override def >(that: Prediction): Boolean = that.score > this.score

  override def <(that: Prediction): Boolean = !(that > this)

  override def compare(that: Prediction): Int = {
    if(this < that) -1
    else if(this > that) 1
    else 0
  }

  override def toString(): String = s"<Prediction $name ($score)>"
}
