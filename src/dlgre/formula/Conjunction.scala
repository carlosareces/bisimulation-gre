package dlgre.formula;

import util.StringUtils.join;

import grapht._;

import dlgre.BitSetSet;

case class Conjunction(sub:List[Formula]) extends Formula {
		sub.foreach { f => 
		  	cantidadExiste += f.cantidadExiste;
		  	categorias = categorias.union(f.categorias); // Categoria de Ex-r1(*)^Ex-r2(*) es {r1, r2}.
		  };

		override def isSatisfied(u:String, graph:GraphT[String,String]) = {
          sub.forall { f => f.isSatisfied(u,graph) }  
        }
		
  		override def toString = {
			join(sub.map { f => f.toString }, " , ");
        } 
		
  		//override def rolesUsados = {
	    //      join(sub.map { f => f.rolesUsados });
	    //} 
          
		override def prettyprint = {
          join(sub.map { f => f.prettyprint }, " & ");
        }
        
        override def flatten = {
          val results = for( x <- sub if ! x.isInstanceOf[Top] ) yield {
            val f = x.flatten;
            
            f match {
              case Conjunction(l) => l
              case _ => List(f)
            }
          }
          
          conjoin(results.flatten { x => x })
        }
        
        override def setToExtension(set:BitSetSet[String], graph:GraphT[String,String]) : Unit = {
          sub match {
            case Nil => set.addAll(graph.getAllNodes);
            case h::t => {
              val tmp = graph.getNodeSet
              h.setToExtension(set, graph);
              t.foreach { sub => sub.setToExtension(tmp, graph); set.intersectWith(tmp); }  
            }
          }
        }
        
        def getSubAsJava = {
          val ret = new java.util.ArrayList[Formula]();
          sub.foreach { x => ret.add(x) };
          ret;
        }
}
