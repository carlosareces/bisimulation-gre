package dlgre.formula;

import grapht._;

import dlgre.BitSetSet;

case class Negation(sub:Formula) extends Formula {
  	override def isSatisfied(u:String, graph:GraphT[String,ProbRelation]) = {
          ! sub.isSatisfied(u,graph)  
        }
          
	override def prettyprint = {
   		"~" + sub.prettyprint       
        }
        
        override def flatten = {
          Negation(sub.flatten)
        }
        
        override def setToExtension(set:BitSetSet[String], graph:GraphT[String,ProbRelation]) : Unit = {
          sub.setToExtension(set, graph);
          set.complement();
        }
}
