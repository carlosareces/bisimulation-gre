package dlgre.formula;

import grapht._;

case class Top extends Formula {
	override def isSatisfied(u:String, graph:GraphT[String,String]) = {
   		true       
        }
        
        override def prettyprint = {
          "T"
        }
        
        override def flatten = {
          this
        }
        
        override def setToExtension(set:BitSetSet[String], graph:GraphT[String,String]) : Unit = {
          set.addAll(graph.getAllNodes);
        }
  
}
