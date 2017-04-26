package dlgre.formula;

import grapht._;
import scala.collection.mutable._;
import dlgre.BitSetSet;

case class Existential(role:String, sub:Formula) extends Formula {
	cantidadExiste = sub.cantidadExiste + 1;
	//categorias = new HashSet() + role; // Categoria de Ex-r(*) es {r}.
	if (role == "in"){
		categorias = sub.categorias;
	} else {
		categorias = new HashSet() + role;
	}
  	override def isSatisfied(u:String, graph:GraphT[String,String]) = {
		graph.getAllNodes.exists { v => graph.hasEdge(u,role,v) && sub.isSatisfied(v,graph) };            
        }
          
  		override def toString = {
  		  if (role == "terminal") {
  			  ""
  		  } else if (sub == Top()) {
  			  role
  		  } else {  //Ex-small.(Ex-terminal.(T)) & Ex-green.(Ex-terminal.(T)) & Ex-cube.(T): s6
  			  role + "( " + sub.toString + ")";
  		  }
        } 
  		
  		override def prettyprint = {
          "Ex-" + role + ".(" + sub.prettyprint + ")";       
        }
        
        override def flatten = {
          Existential(role, sub.flatten)
        }
        
        override def setToExtension(set:BitSetSet[String], graph:GraphT[String,String]) : Unit = {
          val subExtension = sub.extension(graph).asScalaCollection;
          
          set.clear();
          
          subExtension.foreach { v => 
            graph.foreachInEdgeWithRole(v, {(u:String,r:String) => if( r == role ) { set.add(u) }});
          }
        }
}
