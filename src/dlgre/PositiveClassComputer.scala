package dlgre;

import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import dlgre.formula._
import grapht._;
import java.util.ArrayList

import org.jgrapht.graph._;
import scala.collection.mutable._;
import scala.util.Random;
import util.StringUtils.join;

import collection.JavaConversions._;

class PositiveClassComputer(graph:GraphT[String,String],
                            rolesToProbUse: Map[String,Float],
                            rolesToProbDisc: Map[String,Float],
							rolesOrdenados: List[String],
							informative: Boolean) {
  val classes = new ClassContainer(graph);
  
  def compute = {//esta parte no se ejecuta ya que son todas relaciones
    val simplifier = new dlgre.formula.Simplifier(graph);
    // initialize predicates
    graph.getAllPredicates.foreach { p =>
      classes.add(new Literal(p,true), new Literal(p,true));
    }
    //RA: Probabilities for roles from modelos/order.txt
      
    var madeChanges = true;
    /*val s = scala.io.Source.fromFile("modelos/order.txt")
    s.getLines.foreach( line => {
        var sp = line.split(" -> ");
    	//RA: adding line specting first element string "->" second element(double)
    	rolesToProb(sp.apply(0)) = sp.apply(1).toDouble;
    })
    print ("ROLES _TO _PROB: ", rolesToProb);*/   
    // iterate over roles
    //RA: here need to add-rename the condition of stop
    var iteration: Int = 0;
    val maxIteration: Int = 500;
    while( madeChanges && !classes.isAllSingletons ){//&& (iteration < maxIteration) ) {
      
      iteration += 1;
      
      madeChanges = false;
      
      //println("----------------------------- " + iteration);

      //RA: Para cada relacion le asignamos un booleano diciendo si esta relacion ya se uso.
      var rel_used = new HashMap[String,Boolean]()
      rolesOrdenados.foreach{ r => rel_used += r -> false; }
     
      try {
    	  rolesOrdenados.foreach{ r =>
    	    if (!rel_used(r)) {
    	        
	        	val rand1: Float = Math.abs(new Random().nextFloat()); 
	        	val rand2: Float = Math.abs(new Random().nextFloat()); 
	        
	        	var p_use: Float = 1;
	        	var p_disc: Float = 1;
	        
	        	try { p_use = rolesToProbUse(r); }
	        	catch { case e: Exception => 
	        	  
	        	  if (informative == true) {
	          	    println(r + " no tiene p_use: usando 1"); }
	        	  }
	        	try { p_disc = rolesToProbDisc(r); }
	        	catch { case e: Exception => 
	        	  if (informative == true) {
	        	    println(r + " no tiene p_disc: usando 1"); }
	        	  }
	      		if (rand1 <= p_use) {
	      			classes.getClasses.foreach { cl =>
	      				//println ("RANDOM " + rand1 + " " + rand2 );
	   					if (rand2 <= p_disc) {
	   					    //if (informative == true) {
	   					    //  println("relacion: "+r + " P_disc: "+p_disc+" rand2:"+rand2 );
	   					    //}  
	      					if( classes.add(new Existential(r, cl.e1.formula), new Existential(r, cl.e2.formula)) ) {
	      						madeChanges = true;       
	      						rel_used += r -> true;
	      						throw new Exception("break");
	     	  				}
	      				}
	      				else {
	      					if( classes.add(new Top(), new Existential(r, cl.e2.formula)) ) {
	      						madeChanges = true;       
	      						rel_used += r -> true;
	      						throw new Exception("break");
	     	  				}
	      				}      			
	      			}
	      		}
	      		else {
	      		  if (informative == true) {
	      		    println(r + ", no dio la probabilidad de uso, rand: "+ rand1+" Prob_uso: "+ p_use);
	      		  }  
	      		  madeChanges = true;
	      		}
    	    }
    	  }
      }
      catch { case e: Exception =>  }
    }
    //println ("Iteration: "+iteration);
    //print("[max=" + classes.getMaxSize + "]");
    
    classes.getClasses
    
  }
  
  def computeForJava = {
    val ret = new java.util.HashMap[String,Formula]();
    val classes = compute;
    
    classes.foreach { entry =>
    	entry.e1.extension.scalaIterator.foreach { x =>
        	ret.put(x, entry.e2.formula);    
        }
    }
    //print ("RET: ",ret);
    ret
  }
}
