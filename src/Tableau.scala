/**
 * Tableau Algorithm for checking satisfiability of ALC concepts.
 *
 * @author Kevin Warrick
 */

object Tableau {
  /**
   * Convert a concept to Negation Normal Form, which
   * only allows negation on atomic concepts.
   *
   * Examples of rewriting rules:
   *  ¬¬C = C
   *  ¬(C1 ⊓ C2) = (¬C1 ⊔ ¬C2) 
   *  ¬(C1 ⊔ C2) = (¬C1 ⊓ ¬C2) 
   *  ¬∃R.C = ∀R.¬C
   *  ¬∃R.C = ∃R.¬C
   */
  def nnf(concept: Concept): Concept = concept match {
    case Not(Not(c))     => nnf(c)
    case Not(And(x, y))  => Or(Not(nnf(x)), Not(nnf(y)))
    case Not(Or(x, y))   => And(Not(nnf(x)), Not(nnf(y)))
    case Not(Some(r, c)) => Only(r, Not(nnf(c)))
    case Not(Only(r, c)) => Some(r, Not(nnf(c)))
    case And(x, y)       => And(nnf(x), nnf(y))
    case Or(x, y)        => Or(nnf(x), nnf(y))
    case Some(r, c)      => Some(r, nnf(c))
    case Only(r, c)      => Only(r, nnf(c))
    case _ => concept
  }

  def main(args: Array[String]) {
    println("input : "+ args(0))
    val concept = ManchesterParser(args(0)) 
    println(concept)
    println(nnf(concept))
  }
}

/**
 * L is the logical tableau function that maps node and edge labels
 * to a set of concepts and roles, respectively.
 * L(x) = {Concept, ...}
 * L(x,y) = {Role, ...}
 */
class L {
  def apply(x: Concept): List[Concept] = {
    null
  }

  def apply(x: Concept, y: Concept): List[Role] = {
    null
  }
}



