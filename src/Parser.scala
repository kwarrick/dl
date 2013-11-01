/** 
 * ManchesterParser parses subset of syntax used by the ALC Description Logic. 
 * http://www.w3.org/2007/OWL/wiki/ManchesterSyntax#The_Grammar
 *
 * person ⊓ ∀eats.plant ⊓ (¬person ⊔ ∃eats.(¬plant ⊓ ¬dairy))
 * person AND (eats ONLY plant) AND (NOT person OR eats ONLY (plant AND dairy))
 * 
 */

import scala.util.parsing.combinator._
 
class ManchesterParser extends JavaTokenParsers {
  def description: Parser[Any] = conjunction ~ rep("OR" ~ conjunction) | 
                                 conjunction
  def conjunction: Parser[Any] = ident ~ "THAT" ~ opt("NOT") ~ restriction ~ rep("AND" ~ opt("NOT") ~ restriction) | 
                                 primary ~ "AND" ~ primary ~ rep("AND" ~ primary) |
                                 primary
  def primary: Parser[Any] =  opt("NOT") ~ (restriction | atomic)
  def restriction: Parser[Any] =  ident ~ "SOME" ~ primary |
                                  ident ~ "ONLY" ~ primary 
  def atomic: Parser[Any] = ident | 
                            "{" ~ repsep(ident, ",") ~ "}" |
                            "(" ~ description ~ ")" 

}
 
object ParseExpr extends ManchesterParser {
  def main(args: Array[String]) {
    println("input : "+ args(0))
    println(parseAll(description, args(0)))
  }
}
