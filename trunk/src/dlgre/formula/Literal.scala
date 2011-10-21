package dlgre.formula;

import grapht._;

case class Literal(p:String, polarity:Boolean) extends Formula {
  	override def isSatisfied(u:String, graph:GraphT[String,String]) = {
		graph.hasPredicate(u,p) == polarity;            
        }
  
	override def prettyprint = {
   	   if( polarity ) {
       		p	              
           } else {
                "~" + p
           }
        }
        
        override def flatten = {
          this
        }
        
        override def setToExtension(set:BitSetSet[String], graph:GraphT[String,String]) : Unit = {
          set.clear();
          
          set.addAll(graph.getAllNodes.filter {u => graph.hasPredicate(u,p) == polarity});
        }
}
