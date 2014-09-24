package xyz.seto.obscene

import xyz.seto.obscene.utils.Rectangle


class GestureStroke(val points: List[GesturePoint]){
  private def createBoundingBox(cBox: Rectangle, cPoints: List[GesturePoint]): Rectangle = cPoints match {
      case cP :: cPoints => createBoundingBox(cBox.union(cP.point), cPoints)
      case Nil => cBox
  }

  val boundingBox: Rectangle = createBoundingBox(new Rectangle(0, 0), points)

}

