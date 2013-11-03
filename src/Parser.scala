/** 
 * ManchesterParser parses a subset of Manchester Description Logic syntax 
 * used by the ALC Description Logic.
 *
 * Note that this parser parses only concepts, not general concept inclusions,
 * because the Tableau algorithm reasons w.r.t an empty Tbox; a Tbox can be 
 * 'internalized' into a single concept.
 *
 * @author Kevin Warrick
 * @see http://www.w3.org/2007/OWL/wiki/ManchesterSyntax#The_Grammar
 *
 * Example: 
 * person ⊓ ∀eats.plant ⊓ (¬person ⊔ ∃eats.(¬plant ⊓ ¬dairy))
 * person AND (eats ONLY plant) AND (NOT person OR eats ONLY (plant AND dairy))
 * 
 */

import scala.util.parsing.combinator._
 
object ManchesterParser extends JavaTokenParsers {
  def description: Parser[Concept] =
    rep1sep(conjunction, "OR") ^^ (_.reduceLeft(Or))
    conjunction

  def conjunction: Parser[Concept] = 
    rep1sep(primary, "AND") ^^ (_.reduceLeft(And))
    primary

  def primary: Parser[Concept] = 
    "NOT" ~> restriction ^^ (Not(_)) | 
    "NOT" ~> atomic ^^ (Not(_)) |
    restriction | 
    atomic

  def restriction: Parser[Concept] = 
    (ident ~ "SOME" ~ primary) ^^ 
      { case r ~ "SOME" ~ c => Some(Role(r), c) } |
    (ident ~ "ONLY" ~ primary) ^^
      { case r ~ "ONLY" ~ c => Only(Role(r), c) } 

  def atomic: Parser[Concept] = 
    ident ^^ (Atom(_)) | 
    "(" ~> description <~ ")" 

  def apply(input: String): Concept = {
    parseAll(description, input) match {
      case Success(result, _) => result
      case failure : NoSuccess => scala.sys.error(failure.msg)
    }
  }

  def main(args: Array[String]) {
    println("input : "+ args(0))
    println(parseAll(description, args(0)))
  }
}
 
