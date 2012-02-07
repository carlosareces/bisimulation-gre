package dlgre;

import scala.collection.mutable.Queue
import java.io._
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
    
    val log = new FileWriter("log-test.txt") ;
    
    while( madeChanges && !classes.isAllSingletons ){//&& (iteration < maxIteration) ) {
      print("En while mientras haya cambios y no sean todos singletones------------------\n\n");
          
      madeChanges = false;
      
      //print("----------------------------- " + iteration);

      //RA: Para cada relacion le asignamos un booleano diciendo si esta relacion ya se uso.
      var rel_used = new HashSet[String]()
                 
      try {
    	  rolesOrdenados.foreach{ r =>
    	    print("\t\tROL: " + r + "\n");
    	    if (!rel_used.contains(r)) {
    	        
	        	val rand1: Float = Math.abs(new Random().nextFloat()); 
	        	val rand2: Float = Math.abs(new Random().nextFloat()); 
	        
	        	var p_use: Float = 1;
	        	var p_disc: Float = 1;
	        	print("\t Probando el rol (ordenados por prob-uso): " + r + "\n");
	        		
	        	try { p_use = rolesToProbUse(r); }
	        	catch { case e: Exception => 
	        	  
	        	  print(r + " no tiene p_use: usando 1\n"); }
	        	
	        	try { p_disc = rolesToProbDisc(r); }
	        	catch { case e: Exception => 
	        	  print(r + " no tiene p_disc: usando 1\n");
	        	}
	      		if (rand1 <= p_use) {
	      			print("\tDio prob_uso: " + p_use + " mayor que rand1: " + rand1 + " reviso las clases...\n");
	      			classes.getClasses.foreach { cl =>
	      			  	
	   					print("\tClase: " + cl.toString + "\n");
	   					print("\tRelacion: " + r + " P_disc: " + p_disc + " rand2:" + rand2 + "\n" );
	   					
   					    cl.e2.foreach { entry =>
	      			  	  if (rand2 <= p_disc) {
	   					    print("\t\tDiscernible. fi_O: " + new Existential(r, cl.e1.formula).prettyprint+"\n");
	   					    
	   					    	print("\t\t\tfi_R -> " + new Existential(r, entry.formula).prettyprint);
		      					if( classes.add(new Existential(r, cl.e1.formula), new Existential(r, entry.formula)) ) {
		      						madeChanges = true;       
		      						rel_used += r;
		      						print(", se agrego\n");
		      						throw new Exception("break");
		     	  				}
		      					else {
		      						print(", no se agrego\n");
		      					}
	      				  }
	      				  else {
	      				    print("\t\tNo discernible, formula: (Top, " + new Existential(r, entry.formula).prettyprint + ") ");
	      					if( classes.add(new Top(), new Existential(r, entry.formula)) ) {
	      						madeChanges = true;       
	      						rel_used += r;
	      						print(" se agrega\n");
	      						throw new Exception("break");
	     	  				}
	      					else {
	      						print(" no se agrega\n");
	      					}  
	      				  }
	      				}
	      			}
	      		}
	      		else {
	      		  print(r + ", no dio la probabilidad de uso, rand: "+ rand1+" Prob_uso: "+ p_use);
	      		    
	      		  madeChanges = true;
	      		}
    	    }
    	  }
      }
      catch { case e: Exception =>  }
    }
    print("\t\tClases que quedaron: " + classes.getClasses);
    log.close();
    classes.getClasses
    
  }
  
  def computeForJava = {
    val ret = new java.util.HashMap[String,Formula]();
    val classes = compute;
    
    classes.foreach { entry =>
    	entry.e1.extension.scalaIterator.foreach { x =>
        	ret.put(x, entry.e1.formula);    
        }
    }
    ret
  }
}
