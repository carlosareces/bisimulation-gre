package dlgre;

import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import dlgre.formula._
import grapht._;
import java.util.ArrayList

import org.jgrapht.graph._;
import scala.collection.mutable._;
import util.StringUtils.join;

import collection.JavaConversions._;

class PositiveClassComputer(graph:GraphT[String,String], listaRoles:List[String]) {
  val classes = new ClassContainer(graph);
  //val rolesToProb = new HashMap[String,Double]
  
  def compute = {//esta parte no se ejecuta ya que son todas relaciones
    val simplifier = new dlgre.formula.Simplifier(graph);
    // initialize predicates
    graph.getAllPredicates.foreach { p =>
      classes.add(new Literal(p,true));
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
      
      
      print(".");
      //println("-----------------------------");
      //println(classes.classesGraph);

      //RA: Parse order from a file, the file need to be rol enter rol enter ...
      //var li = new HashMap[V,Double]()
     
      //RA: Here I put reverse, because the last element that I added was the first one in the list
      graph.getAllRoles.foreach{ r =>
      	classes.getClasses.foreach { cl =>
      	  //print ("CLASE: ",cl);
          if( classes.add(new Existential(r,cl.formula)) ) {
          	madeChanges = true;       
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
    	entry.extension.scalaIterator.foreach { x =>
        	ret.put(x, entry.formula);    
        }
    }
    print ("RET: ",ret);
    ret
  }
}
