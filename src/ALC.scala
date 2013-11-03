/** 
 * ALC syntax modeled functionally as case classes.
 * @author Kevin Warrick
 */

case class Role(ident: String)
case class Atom(ident: String) extends Concept
case class Not(c: Concept) extends Concept
case class And(x: Concept, y: Concept) extends Concept
case class Or(x: Concept, y: Concept) extends Concept
case class Some(r: Role, c: Concept) extends Concept
case class Only(r: Role, c: Concept) extends Concept 

class Concept {
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
    def nnf(): Concept { 

    }
}
