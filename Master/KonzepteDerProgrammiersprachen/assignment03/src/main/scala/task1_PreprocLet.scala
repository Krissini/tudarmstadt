import definitions.FAEBase._
import definitions.FLAEBase._

// --------------------------------------------
// --- PUT ALL YOUR CHANGES BELOW THIS LINE ---
// --------------------------------------------

object PreprocLet {
  def preprocLet(expr: FLAE): FAE = expr match {
    case Let(boundId, namedExpr, boundExpr) => {
      val substNamedExpr = subst(namedExpr, substId, value)
      if (boundId == substId)
        Let(boundId, substNamedExpr, boundExpr)
      else
        Let(boundId, substNamedExpr, subst(boundExpr, substId, value))
    }
  }
}
