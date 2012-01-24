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
                            rolesToProbDisc: Map[String,Float]) {
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
    while( madeChanges && !classes.isAllSingletons ) {
      madeChanges = false;
      
      //println("\n\n\n\n\nClasses:");
      //classes.getClasses.foreach { fmla => println(simplifier.removeConjunctionsWithTop(fmla).prettyprint + ": " + util.StringUtils.join(fmla.extension(graph),",")) }
      
      
      //println("-----------------------------");
      //println(classes.classesGraph);

      //RA: Parse order from a file, the file need to be rol enter rol enter ...
      //var li = new HashMap[V,Double]()
     
      //RA: Here I put reverse, because the last element that I added was the first one in the list
      graph.getAllRoles.foreach{ r =>
        
        val rand1: Float = Math.abs(new Random().nextFloat()) / Float.MaxValue;
        val rand2: Float = Math.abs(new Random().nextFloat()) / Float.MaxValue;
        
        var p_use: Float = 1;
        var p_disc: Float = 1;
        
        try { p_use = rolesToProbUse(r) }
        catch { case e: Exception => println(r + " no tiene p_use: usando 1"); }
        
        try { p_disc = rolesToProbDisc(r) }
        catch { case e: Exception => println(r + " no tiene p_disc: usando 1"); }
        
      	if (rand1 <= p_use) {
      		classes.getClasses.foreach { cl =>
   				//print ("CLASE: ",cl);
      			if (rand2 <= p_disc) {
      				if( classes.add(new Existential(r, cl.e1.formula), new Existential(r, cl.e2.formula)) ) {
      					madeChanges = true;       
     	  			}
      			}
      			else {
      				if( classes.add(new Top(), new Existential(r, cl.e2.formula)) ) {
      					madeChanges = true;       
     	  			}
      			}
      		}
      	}
      }
    }
    
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
    print ("RET: ",ret);
    ret
  }
}
