/** 
 * ALC syntax modeled functionally as case classes.
 * @author Kevin Warrick
 */

case class Role(ident: String)
abstract class Concept
case class Atom(ident: String) extends Concept
case class Not(c: Concept) extends Concept
case class And(x: Concept, y: Concept) extends Concept
case class Or(x: Concept, y: Concept) extends Concept
case class Some(r: Role, c: Concept) extends Concept
case class Only(r: Role, c: Concept) extends Concept 

