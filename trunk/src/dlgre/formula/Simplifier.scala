package dlgre.formula;

import grapht._;

import scala.collection.mutable._;

class Simplifier(graph:GraphT[String,String]) {
	private val _extension = new HashMap[Formula,Set[String]];
        
        
        
        
        
        private def extension(fmla:Formula) : Set[String] = {
          if( ! _extension.contains(fmla) ) {
            val e = new HashSet[String];
            
            // println("[recompute " + fmla + "]");
            
            graph.getAllNodes.foreach { u =>
              if( fmla.isSatisfied(u,graph) ) {
                e += u;
              }
            }
            
            _extension += fmla -> e;
          }
          
          _extension.get(fmla).get;
        }
        
        private def extension(l : List[Formula]) : Set[String] = {
          val ext = new HashSet[String];
          
          ext ++= graph.getAllNodes;
          l.foreach { f => ext intersect extension(f) }
          
          ext
        }
        
        
        
        def simplify(fmla : Formula) = {
           fmla.flatten.simplifyConjunctions({ l => removeEntailedFormulas(stripEntailedByPositive(l), Nil) })
        }
        
        
        
        private def stripEntailedByPositive(l : List[Formula]) = {
          val ext = extension(l)
          
          // all positive atomic concepts that are true for all individuals in the extension
          val positives = (for( p <- graph.getAllPredicates if ext subsetOf extension(Literal(p,true)) ) 
            			yield Literal(p,true))
                                    
          val usedPositives = new HashSet[Formula]
          val keptFormulas = new HashSet[Formula]
          
          var foundPositive = false;
          
          l.foreach { f =>
          	foundPositive = false;
                for( p <- positives ) {
                  if( !foundPositive ) {
                    if( extension(p) subsetOf extension(f) ) {
                      foundPositive = true;
                      usedPositives += p;
                    }
                  }
                }
                
                if( !foundPositive ) {
                  keptFormulas += f
                }
          }

          keptFormulas ++= usedPositives
          keptFormulas.toList
        }
        
        private def removeEntailedFormulas(l : List[Formula], accu : List[Formula]) : List[Formula] = {
          if( l.isEmpty ) {
            accu
          } else {
            val fmla = l.head;
            
            if( isEntailed(fmla, l.tail ::: accu) ) {
                removeEntailedFormulas(l.tail, accu);
             } else {
              removeEntailedFormulas(l.tail, fmla::accu)
             }
          }
        }
        
        /*
	** earlier version: remove only entailed negative subformulas
          
          fmla match {
            case Literal(p,false) => if( isEntailed(fmla, l.tail ::: accu) ) {
                                      removeEntailedNegatives(l.tail, accu);
                                     } else {
                                      removeEntailedNegatives(l.tail, fmla::accu)
                                     }
            case Negation(p) => if( isEntailed(fmla, l.tail ::: accu) ) {
                                  removeEntailedNegatives(l.tail, accu);
                                } else {
                                  removeEntailedNegatives(l.tail, fmla::accu)
                                }
            case _ => removeEntailedNegatives(l.tail, fmla::accu)

          }
        */
        
      private def isEntailed(fmla : Formula, others : List[Formula] ) : Boolean = {
        extension(others) subsetOf extension(fmla)
      }
      
      // TODO: This only captures the very special case of a conjunction such that the
      // conjunction of all negative literals in it is exactly coextensive with a single
      // positive atom.
      private def replaceNegativeConjunctions(l : List[Formula]) : List[Formula] = {
        val negativeLiterals = l filter { f => f match { case Literal(p,false) => true; case _ => false }}
        val rest = l remove { f => f match { case Literal(p,false) => true; case _ => false }}
        
        val ext = extension(negativeLiterals)
        val alternatives = (for( p <- graph.getAllPredicates if ext == extension(Literal(p,true)) ) yield Literal(p,true)).toList
        
        if( !negativeLiterals.isEmpty && !alternatives.isEmpty ) {
          alternatives.head :: rest
        } else {
          l
        }
      }
        
}
