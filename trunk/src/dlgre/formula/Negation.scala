package dlgre.formula;

import grapht._;

import dlgre.BitSetSet;

case class Negation(sub:Formula) extends Formula {
	cantidadExiste = sub.cantidadExiste;
	override def isSatisfied(u:String, graph:GraphT[String,String]) = {
          ! sub.isSatisfied(u,graph)  
        }

	override def toString = {
   		"not " + sub.toString       
        }
  	
	override def prettyprint = {
   		"~" + sub.prettyprint       
        }
        
        override def flatten = {
          Negation(sub.flatten)
        }
        
        override def setToExtension(set:BitSetSet[String], graph:GraphT[String,String]) : Unit = {
          sub.setToExtension(set, graph);
          set.complement();
        }
}
