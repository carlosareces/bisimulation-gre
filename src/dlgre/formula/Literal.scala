package dlgre.formula;

import grapht._;

import dlgre.BitSetSet;

case class Literal(p:String, polarity:Boolean) extends Formula {
	cantidadExiste = 1;
	override def isSatisfied(u:String, graph:GraphT[String,String]) = {
		graph.hasPredicate(u,p) == polarity;            
	}

	override def toString = {
   	   if( polarity ) {
       		p	              
           } else {
                "not (" + p + ")"
           }
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
