/**
 * Tableau Algorithm for ALC.
 * 
 * Parallel, concurrent, and distributed 'trace' strategy
 * tableau algorithm for 'internalized' ALC concepts.
 *
 * @author Kevin Warrick
 */

import akka.actor._
import akka.pattern.{ ask, pipe }
import akka.util.Timeout
import scala.util.Random
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.collection.mutable.HashMap
import scala.collection.SortedSet

case class Expand(c: Concept) 
case class Duplicate(x: SortedSet[Concept], xy: HashMap[Role, List[ActorRef]])
case class Query(c: Concept) 
case class Response(r: Role, c: Concept, b: Boolean) 

class Tableau extends Actor {
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
   * Initialize the completion graph and begin expanding.
   *
   * Algorithm terminates when no more rules can be applied to
   * the completion graph nodes or no nodes exists to expand.
   */
  val root = context.actorOf(Props[Node])
  def receive = {
    case Expand(concept) => {
      root ! Expand(nnf(concept))
    }
    // ...
  }
}

class Node extends Actor {
  import context._
  implicit val timeout = Timeout(60.seconds)

  var concepts = SortedSet[Concept]()(ConceptOrdering)
  var roles = HashMap[Role, List[ActorRef]]()

  /**
   * Apply expansion rules to concept according to 'trace' strategy.
   * (i.e. disjunction, conjunction, existential, universal)
   */ 
  def expand(set: SortedSet[Concept]): Unit = {
    println("Set:")
    println(set)
    println

    if (set.isEmpty) return 
    else set.head match {
      case Or(p, q) => {
        println("Or:")
        println(p)
        println(q)
        println

        if ((concepts intersect Set[Concept](p, q)).isEmpty) {
          parent ! Duplicate(concepts, roles)
          expand(set.tail + p)
          concepts += p
        }
      }
      case And(p, q) => {
        println("And:")
        println(p)
        println(q)
        println

        val diff = Set[Concept](p, q) diff concepts
        concepts += (p, q)
        expand(set.tail ++ diff)
      }
      case Some(r, c) => {
        println("Some:")
        println(r)
        println(c)
        println

        if (roles.getOrElse(r, Nil).isEmpty) {
          val child = actorOf(Props[Node]) 
          roles(r) = List(child)
          println(roles)
          child ! Expand(c)
        }
        else {
          // BROKEN: RACE CONDITION!
          // val seq = roles(r) map (ask(_, Query(c)).mapTo[Boolean])
          // val any = Future.find(seq) { res => res }
          // any.map(res => Response(r, c, res.getOrElse(false))).pipeTo(self)
        }
        expand(set.tail)
      }
      case Only(r, c) => {
        println("Only:")
        println(r)
        println(c)

        println("roles:")
        println(roles)
        println

        roles(r) map (_ ! Expand(c))
        expand(set.tail)
      }
      case c =>  {
        println(c)
        println
        concepts += c
        expand(set.tail)
      }
    }
  }

  def expand(concept: Concept): Unit = {
    if (!(concepts contains concept)) {
      concepts += concept
      expand(SortedSet(concept)(ConceptOrdering))
    }
  }

  /**
   * Subset blocking strategy ensures termination by preventing 
   * cyclic application of expansion rules. Node x is blocked if
   * there exists an ancestor node y of x in the tree such that
   * L(x) is a subset of L(y).
   */
  // TODO: IMPLEMENT SUBSET BLOCKING

  def receive = {
    case Expand(concept) => {
      println(self.path + " Expand: ")
      println(concept)
      expand(concept) 
    }
    case Query(concept) => {
      println("Query: " + concept)
      sender ! (concepts contains concept)
    }
    case Duplicate(concepts, roles) => {
      println("not implemented")
    }
  }
}

object Main {
  def main(args: Array[String]) {
    println("input : "+ args(0))
    val concept = ManchesterParser(args(0))
    println(concept)

    val system = ActorSystem()
    val root = system.actorOf(Props[Tableau], "tableau")
    root ! Expand(concept)
  }
}

