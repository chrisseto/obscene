package xyz.seto.obscene.libgdx;

import xyz.seto.obscene.Gesture


trait GestureListener {
  def onGesturePreformed(view: GestureDetector, gesture: Gesture): Boolean
}
