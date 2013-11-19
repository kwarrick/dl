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
import scala.collection.mutable.TreeSet
import scala.collection.mutable.SortedSet
import scala.collection.mutable.HashMap

case class Satisfy(c: Concept)
case class Expand(e: SortedSet[Concept], u: SortedSet[Concept])
case class Duplicate(e: SortedSet[Concept], u: SortedSet[Concept])
case class Blocked(s: SortedSet[Concept])

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
    case Not(And(p, q))  => Or(nnf(Not(p)), nnf(Not(q)))
    case Not(Or(p, q))   => And(nnf(Not(p)), nnf(Not(q)))
    case Not(Some(r, c)) => Only(r, nnf(Not(c)))
    case Not(Only(r, c)) => Some(r, nnf(Not(c)))
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
    case Satisfy(concept) => root ! Satisfy(nnf(concept))
    case "Complete"       => println("Satisfiable")
    case "Clash"          => println("Unsatisfiable")
  }
}

class Node extends Actor {
  val log = Logging(context.system, this)

  var count = 0
  val expanded = SortedSet[Concept]()(ConceptOrdering)
  val unexpanded = SortedSet[Concept]()(ConceptOrdering)
  
  val branches = HashMap[ActorRef, Int]()
  val duplicates = HashMap[ActorRef, ActorRef]()

  /**
   * Unwind duplicate chain to find associated branch reference.
   */
  def branch(a: ActorRef): ActorRef = {
    if (branches contains a) a
    else branch(duplicates(a))
  }

  /**
   * Determine if a set contains inconsistent concepts (i.e. {C, ¬C}).
   */
  def clash(s: SortedSet[Concept]): Boolean = {
    def pairs = for (p <- s; q <- s) yield (p, q)
    pairs.find(p => p._1 == Not(p._2)).nonEmpty
  }

  /**
   * Apply expansion rules to concept set according to 'trace' strategy.
   * (i.e. disjunction, conjunction, existential, universal)
   */ 
  def expand(e: SortedSet[Concept], u: SortedSet[Concept]) = {
    expanded ++= e
    unexpanded ++= u
    if (expanded.nonEmpty) context.parent ! Blocked(expanded)

    while (unexpanded.nonEmpty) {
      val concept = unexpanded.head
      unexpanded.remove(concept)

      if (clash(expanded)) unexpanded.clear
      else concept match {
        case Atom(s)      => expanded.add(concept)
        case Not(Atom(s)) => expanded.add(concept)
        case Or(p, q) => {
          if (!(expanded contains p) && !(expanded contains q)) {
            context.parent ! Duplicate(expanded.clone, unexpanded + p)
            unexpanded.add(q)
          }
        }
        case And(p, q) => {
          unexpanded.add(p)
          unexpanded.add(q)
        }
        case Some(r, c) => {
          val only = unexpanded.collect({case Only(s, q) if s == r => q})
          val set = SortedSet[Concept]()(ConceptOrdering) 
          val child = context.actorOf(Props[Node]) 
          child ! Expand(set, set ++ only + c)
          branches += (child -> 1)
          count += 1
        }
        case Only(r, c) => concept
      }
    }
    if (clash(expanded)) {
      log.info("clash: " + expanded)
      context.parent ! "Clash"
    }
    else if (count == 0) context.parent ! "Complete"
  }

  def receive = {
    case Satisfy(concept) => {
      count = 1
      val set = SortedSet[Concept]()(ConceptOrdering)
      val child = context.actorOf(Props[Node]) 
      child ! Expand(set, set + concept) 
      branches += (child -> 1)
    }
    case Expand(e, u) => { 
      expand(e, u)
    }
    case Duplicate(e, u) => {
      val child = context.actorOf(Props[Node]) 
      child ! Expand(e, u)
      duplicates += (child -> sender)
      branches(branch(sender)) += 1
      count += 1
    }
    case Blocked(s) => {
      if (s subsetOf expanded) sender ! "Blocked"
      else context.parent forward Blocked(s)
    }
    case "Clash" => {
      count -= 1
      context.stop(sender)
      branches(branch(sender)) -= 1
      if (branches.exists(_._2 == 0)) {
        log.info("fatal clash: " + branches)
        log.info(sender)
        context.parent ! "Clash"
      }
      else if (count == 0) context.parent ! "Complete"
    }
    case "Complete" => {
      count -= 1
      context.stop(sender)
      if (count == 0) context.parent ! "Complete"
    }
    case "Blocked" => {
      context.parent ! "Complete"
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
    reasoner ! Satisfy(concept)
  }
}

