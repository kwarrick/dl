/**
 * Tableau Algorithm for ALC.
 * 
 * Parallel, concurrent, and distributed 'trace' strategy
 * tableau algorithm for 'internalized' ALC concepts.
 *
 * @author Kevin Warrick
 */

import akka.actor._
import akka.event.Logging
import scala.collection.SortedSet
import scala.collection.mutable.HashMap

case class Expand(c: Concept) 
case class ExpandSome(s: Set[Concept])
case class ExpandOr(e: SortedSet[Concept], u: SortedSet[Concept])
case class Duplicate(e: SortedSet[Concept], u: SortedSet[Concept])
case class Blocked(s: SortedSet[Concept])
case class Unblock()

class Tableau extends Actor {
  val log = Logging(context.system, this)

  /**
   * Convert a concept to Negation Normal Form, which
   * only allows negation on atomic concepts.
   *
   * Rewriting rules:
   *  ¬¬C = C
   *  ¬(C1 ⊓ C2) = (¬C1 ⊔ ¬C2) 
   *  ¬(C1 ⊔ C2) = (¬C1 ⊓ ¬C2) 
   *  ¬∃R.C = ∀R.¬C
   *  ¬∃R.C = ∃R.¬C
   */
  def nnf(concept: Concept): Concept = concept match {
    case Not(Not(c))     => nnf(c)
    case Not(And(p, q))  => Or(Not(nnf(p)), Not(nnf(q)))
    case Not(Or(p, q))   => And(Not(nnf(p)), Not(nnf(q)))
    case Not(Some(r, c)) => Only(r, Not(nnf(c)))
    case Not(Only(r, c)) => Some(r, Not(nnf(c)))
    case And(p, q)       => And(nnf(p), nnf(q))
    case Or(p, q)        => Or(nnf(p), nnf(q))
    case Some(r, c)      => Some(r, nnf(c))
    case Only(r, c)      => Only(r, nnf(c))
    case _ => concept
  }

  /**
   * Initialize the completion graph and begin expanding concept.
   *
   * Algorithm terminates when no more rules can be applied to
   * the completion graph nodes or no nodes exists to expand.
   */
  val root = context.actorOf(Props[Node])
  def receive = {
    case Expand(concept) => 
      root ! Expand(nnf(concept))
    case "Complete" =>
      println("Satisfiable")
    case "Clash" =>
      println("Unsatisfiable")
  }
}

class Node extends Actor {
  var count = 0
  val log = Logging(context.system, this)
  var concepts = SortedSet[Concept]()(ConceptOrdering)
  var duplicates = HashMap[Set[ActorRef], Int]()

  /**
   * Determine if a set contains clashing concepts (i.e. {C, ¬C}).
   */
  def clash(s: SortedSet[Concept]): Boolean = {
    def pairs = for (p <- s; q <- s) yield (p, q)
    pairs.find(p => p._1 == Not(p._2)).nonEmpty
  }

  /**
   * Apply expansion rules to concept set according to 'trace' strategy.
   * (i.e. disjunction, conjunction, existential, universal)
   */ 
  def expand(expanded: SortedSet[Concept], unexpanded: SortedSet[Concept]): 
    SortedSet[Concept] = {

    if (unexpanded.isEmpty) 
      expanded
    else if (clash(expanded) || clash(unexpanded)) 
      expanded ++ unexpanded
    else unexpanded.head match {
      case Or(p, q) => {
        if ((expanded intersect Set(p, q)).isEmpty) {
          context.parent ! Duplicate(expanded, unexpanded.tail + p)
          expand(expanded, unexpanded.tail + q)
        }
        else expand(expanded, unexpanded.tail)
      }
      case And(p, q) => {
        val diff = Set[Concept](p, q) diff expanded
        return expand(expanded ++ diff, unexpanded.tail ++ diff)
      }
      case Some(r, c) => {
        val only = unexpanded.collect({case Only(s, q) if s == r => q})
        context.actorOf(Props[Node]) ! ExpandSome(only.toSet + c)
        count += 1
        expand(expanded, unexpanded.tail)
      }
      case _ => {
        expanded ++ unexpanded
      }
    }
  }

  /**
   * Subset blocking strategy ensures termination by preventing 
   * cyclic application of expansion rules. 
   * Node x is blocked if there exists an ancestor node y of x
   * in the tree such that L(x) is a subset of L(y).
   */
  // TODO: IMPLEMENT SUBSET BLOCKING
  // def blocked: Receive = {
  // }

  def receive = {
    case Expand(concept) => {
      val s = SortedSet(concept)(ConceptOrdering)
      concepts = expand(s, s)
      if (clash(concepts)) context.parent ! "Clash"
      else context.parent ! "Complete"
    }
    case ExpandSome(set) => {
      val sorted = SortedSet[Concept]()(ConceptOrdering) ++ set 
      concepts = expand(sorted, sorted)
      if (clash(concepts)) context.parent ! "Clash"
      else context.parent ! "Complete"
    }
    case ExpandOr(e: SortedSet[Concept], u: SortedSet[Concept]) => {
      concepts = expand(e, u)
      if (clash(concepts)) context.parent ! "Clash"
      else context.parent ! "Complete"
    }
    case Duplicate(e: SortedSet[Concept], u: SortedSet[Concept]) => {
      val child = context.actorOf(Props[Node]) 
      child ! ExpandOr(e, u)
      duplicates(Set(child, sender)) = 2
      count += 1
    }
    case "Clash" => {
      for (k <- duplicates.keys if k contains sender) {
        duplicates(k) -= 1    
        if (duplicates(k) == 0) {
          context.parent ! "Clash" 
          context.stop(self)
        }
      }
    }
    case "Complete" => {
      count -= 1
      if (count == 0) context.parent ! "Complete"
    }
    case msg => log.info("unknown message: " + msg)
  }
}

object Main {
  def main(args: Array[String]) {
    println("input : "+ args(0))
    val concept = ManchesterParser(args(0))
    println(concept)

    val system = ActorSystem()
    val reasoner = system.actorOf(Props[Tableau], "tableau")
    reasoner ! Expand(concept)
  }
}

