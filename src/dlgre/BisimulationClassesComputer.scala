package dlgre;

import scala.collection.mutable._;

import dlgre.formula._;
import grapht._;

class BisimulationClassesComputer(graph:GraphT[String,String]) {
   private val extensions = new HashMap[Formula, BitSetSet[String]];
   
   private def getExtension(fmla:Formula) = fmla.extension(graph);
   
   /*
   private def getExtension(fmla:Formula) = {
     if( !extensions.contains(fmla) ) {
       extensions += fmla -> fmla.extension(graph);
     }
     
     extensions.get(fmla).get
   }
   */
   
   
   // Computes the bisimulation classes of a graph.  The method returns a list of Subset objects
   // representing the classes.
   def compute = {
     val roles = graph.getAllRoles;
     
     // the current classes
     val queue = new Queue[Option[Formula]]
     
     // initialize with a single class that contains everything
     queue += Some(new Top());
     
     // split over all (positive) literals up to saturation
     splitOverLiterals(queue, graph.getAllPredicates);
     
     
     // now repeatedly split over roles to distinguished subsets
     var oldQueue : List[Formula] = Nil;
     var newQueue = extractQueue(queue);
     var madeChanges = true;

     while( madeChanges && !allSingleton(newQueue) ) {
       print(".");
       oldQueue = newQueue;
       splitOverRoles(queue, roles);
       newQueue = extractQueue(queue);

       madeChanges = (oldQueue != newQueue);
     }

     /*
     do {
       
       //println("Queue is now: " + newQueue);
     } while( oldQueue != newQueue );
     */

     newQueue;
   }
   
   private def allSingleton(queue:List[Formula]) = {
     queue.forall { f => getExtension(f).size == 1 }
   }
   
   // Splits classes that satisfy different positive literals.
   private def splitOverLiterals(queue: Queue[Option[Formula]], predicates: Collection[String]) = {
     predicates.foreach { p =>
       forallQueue(queue, { (formula, queue) =>
       	splitOverLiteral(formula, p, true) match {
           case Some((sub1,sub2)) => { 
             queue += Some(sub1);
             queue += Some(sub2);
           }
           
           case None => queue += Some(formula);
        }});
     }
   }
   
   private def splitOverLiteral(f:Formula, pred:String, polarity:Boolean) = {
     val ext = getExtension(f);
     
     if( ext.size <= 1 ) {
       None
     } else {
       val sub1 = new Conjunction(List(f, new Literal(pred, polarity)));
       val sub2 = new Conjunction(List(f, new Literal(pred, !polarity)));
       
       if( ext != getExtension(sub1)  &&  ext != getExtension(sub2) ) {
         Some((sub1,sub2))
       } else {
         None
       }
     }
   }
   
   // Splits classes that have the same role pointing into different previously
   // existing classes (1 step). 
   def splitOverRoles(queue : Queue[Option[Formula]], roles : Set[String]) = {
     val elements = extractQueue(queue);
     val localQueue = new Queue[Option[Formula]];
     
     queue.clear;
     
     elements.foreach { el =>
        localQueue.clear;
        localQueue += Some(el);
        
        for( val role <- roles; val sub <- elements ) {
          	//println("[" + role + "/" + sub + "] ");
               forallQueue(localQueue, { (formula, q) =>
               splitOverRole1(formula, role, sub) match {
                 case Some((s1,s2)) => {
                  // println("  - split " + subset + " over " + role + " into " + s1 + " and " + s2);
                   q += Some(s1);
                   q += Some(s2);
                 }
                 
                 case None => q += Some(formula);
               }});
             }
        
        
        queue ++= localQueue;
     }
   }

   private def splitOverRole1(formula:Formula, role:String, roleTo:Formula) = {
     val ext = getExtension(formula)
     
     if( ext.size > 1 &&
       ext.exists { x => getExtension(roleTo).exists { y => graph.hasEdge(x,role,y)} } &&
       ext.exists { x => getExtension(roleTo).forall { y => !graph.hasEdge(x,role,y)} } 
     ) {
       Some((new Conjunction(List(formula, new Existential(role, roleTo))),
             new Conjunction(List(formula, new Negation(new Existential(role, roleTo)))))
           )
     } else {
       None
     }
   }
   
   
   
   
   
   // Calls a function for every element in a queue.  In order to ensure that the queue
   // is only traversed once, a None element is appended to the queue before the iteration.
   // This element is removed afterwards.  Crucially, the function gets passed the entire queue
   // as an argument and is allowed to append new elements to the queue.
   def forallQueue(q : Queue[Option[Formula]], proc : (Formula,Queue[Option[Formula]]) => Unit) = {
     var finished = false;
     q += None;
     
     while(!finished) {
       val el = q.dequeue
       
       el match {
            case None => finished = true;
            case Some(subset) => proc(subset,q);
       }
     }
   }
   
   // Extracts the list of values from a queue that only contains Some elements.
   // The method throws an exception if the queue contains None elements.
   def extractQueue(queue : Queue[Option[Formula]]) = {
     (for( val x <- queue.elements if x.isInstanceOf[Some[Formula]] ) 
       yield (x match { 
         case Some(subset) => subset; 
         case None => throw new Exception("Extracting from queue with empty elements!") 
       })).toList
   }
   
}
