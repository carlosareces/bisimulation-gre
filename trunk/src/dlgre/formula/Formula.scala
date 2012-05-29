package dlgre.formula;

import scala.collection.mutable._;
import dlgre.BitSetSet;
import grapht._;

abstract class Formula {
  		var memoizedExtension : BitSetSet[String] = null;
        
  		var categorias : Set[String] = Set();//new HashSet[String];//null;

  		def toString : String;
  	    
  	    def prettyprint : String;

        def isSatisfied(u:String, graph:GraphT[String,String]) : Boolean;

        def flatten : Formula;
        
        def setToExtension(set:BitSetSet[String], graph:GraphT[String,String]) : Unit;
        
               
        def extension(graph:GraphT[String,String]) = {
          if( memoizedExtension == null ) {
            memoizedExtension = graph.getNodeSet;
            setToExtension(memoizedExtension,graph)  
          }
          
          memoizedExtension
        }
        
        
        def simplifyConjunctions(simplifier : List[Formula] => List[Formula]) : Formula = {
          this match {
            case Conjunction(l) => conjoin(simplifier(l map { x => x.simplifyConjunctions(simplifier)}))

            case Existential(r,sub) => Existential(r,sub.simplifyConjunctions(simplifier))
            case Literal(x,y) => this
            case Negation(sub) => Negation(sub.simplifyConjunctions(simplifier))
            case Top() => this
          }
       }
        
        def roles: Set[String] = {
          var res:HashSet[String] = new HashSet[String]();
          this match {
            case Conjunction(l) => l.foreach { f => res = res.union(f.roles) }
            case Existential(r,sub) => res.add(r)
            case Literal(x,y) => res.add(x)
            case Negation(sub) => 
            case Top() => 
          }
          res
       }
        
        def removeConjunctionsWithTop = {
          flatten.simplifyConjunctions({ l => l.remove { el => el.isInstanceOf[Top] } });
        }
        
        /*
        
        def extension(graph:Graph[String]) : Set[String] = {
          //println("** CALL to extension");
          
          val ext = new HashSet[String];
          graph.getAllNodes.foreach { u =>  if(isSatisfied(u,graph)) { ext += u; }}
          ext
          /*
          
          if( ! memoize.contains(graph) ) {
             val ext = new HashSet[String];
             
             graph.getAllNodes.foreach { u =>  if(isSatisfied(u,graph)) { ext += u; }}
             memoize += graph -> ext
          } else {
            println("*** (memoized)");

          }
          
          memoize.get(graph).get
          */
        }
        */
          
        def conjoin(l : List[Formula]) : Formula = {
          if( l.isEmpty ) {
            Top()
          } else {
            Conjunction(l)
          }
        }
}
