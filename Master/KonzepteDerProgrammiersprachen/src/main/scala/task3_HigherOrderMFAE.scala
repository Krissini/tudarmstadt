import scala.language.implicitConversions
import definitions.MFAEBase._

object HigherOrderMFAE {

  // Some implicit conversions you may find helpful
  implicit def symbolToList(x: Symbol): List[Symbol] = List(x)
  implicit def faeToList(x: MFAE): List[MFAE] = List(x)
  implicit def symbolToFAE(x: Symbol): MId = MId(x)
  implicit def intToFAE(x: Int): MNum = MNum(x)
  implicit def symbolToFAEList(x: Symbol): List[MFAE] = faeToList(symbolToFAE(x))

  // example
  val exampleFun = MFun(List('a, 'b), MAdd('a, 'b))

  // --------------------------------------------
  // --- PUT ALL YOUR CHANGES BELOW THIS LINE ---
  // --------------------------------------------

}
