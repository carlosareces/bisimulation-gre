package dlgre.formula;

import grapht._;
import dlgre.BitSetSet;

case class Top extends Formula {
	override def isSatisfied(u:String, graph:GraphT[String,ProbRelation]) = {
   		true       
        }
        
        override def prettyprint = {
          "T"
        }
        
        override def flatten = {
          this
        }
        
        override def setToExtension(set:BitSetSet[String], graph:GraphT[String,ProbRelation]) : Unit = {
          set.addAll(graph.getAllNodes);
        }
  
}
