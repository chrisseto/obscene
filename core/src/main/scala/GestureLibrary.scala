package xyz.seto.obscene

abstract class GestureLibrary {
    protected val mStore = new GestureStore()

    def load(): Boolean
    def save(): Boolean

    def isReadOnly = false

    def recognize = mStore.recognize _

    def getLearner = mStore.getLearner _

    def removeEntry = mStore.removeEntry _

    def getGestures = mStore.getGestures _

    def getGestureEntries = mStore.getGestureEntries _

    def addGesture = mStore.addGesture _
    def removeGesture = mStore.removeGesture _

    def setSequenceType = mStore.setSequenceType _
    def getSequenceType = mStore.getSequenceType _

    def setOrientationStyle = mStore.setOrientationStyle _
    def getOrientationStyle =  mStore.getOrientationStyle _
}
