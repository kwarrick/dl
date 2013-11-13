/** 
 * ALC syntax modeled functionally as case classes.
 * @author Kevin Warrick
 *
 * Examples:
 * ¬(C1 ⊓ C2)   Not(And(C1, C2))
 * ¬∃R.C        Some(R, C)
 */

case class Role(ident: String)
abstract class Concept
case class Atom(ident: String) extends Concept
case class Not(c: Concept) extends Concept
case class And(x: Concept, y: Concept) extends Concept
case class Or(x: Concept, y: Concept) extends Concept
case class Some(r: Role, c: Concept) extends Concept
case class Only(r: Role, c: Concept) extends Concept 

object ConceptOrdering extends Ordering[Concept] {
  def order(concept: Concept): Int = concept match {
    case Or(p, q)   => 0
    case And(p, q)  => 1
    case Some(r, c) => 2
    case Only(r, c) => 3
    case Atom(s)    => 4
    case _          => -1
  }
  def compare(p: Concept, q: Concept) = 
    Ordering[(Int,Int)].compare((order(p), p.hashCode), (order(q), q.hashCode))
}

