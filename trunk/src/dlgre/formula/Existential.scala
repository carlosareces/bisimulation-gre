package dlgre.formula;

import grapht._;

case class Existential(role:String, sub:Formula) extends Formula {
  	override def isSatisfied(u:String, graph:GraphT[String,String]) = {
		graph.getAllNodes.exists { v => graph.hasEdge(u,role,v) && sub.isSatisfied(v,graph) };            
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
