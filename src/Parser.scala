/** 
 * ManchesterParser parses a subset of Manchester Description Logic
 * syntax used by the ALC Description Logic.
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
 
class ManchesterParser extends JavaTokenParsers {
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
}
 
object ParseExpr extends ManchesterParser {
  def main(args: Array[String]) {
    println("input : "+ args(0))
    println(parseAll(description, args(0)))
  }
}
