package dlgre;

import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import dlgre.formula._
import grapht._;
import java.util.ArrayList

class PositiveClassComputer(graph:GraphT[String,String]) {
  val classes = new ClassContainer(graph);
  
  
  def compute = {
    val simplifier = new dlgre.formula.Simplifier(graph);

    // initialize predicates
    graph.getAllPredicates.foreach { p =>
      classes.add(new Literal(p,true));
    }
    
    // iterate over roles
    var madeChanges = true;
    
    //RA: here need to add-rename the condition of stop
    while( madeChanges && !classes.isAllSingletons ) {
      madeChanges = false;
      
      //println("\n\n\n\n\nClasses:");
      //classes.getClasses.foreach { fmla => println(simplifier.removeConjunctionsWithTop(fmla).prettyprint + ": " + util.StringUtils.join(fmla.extension(graph),",")) }
      
      
      print(".");
      //println("-----------------------------");
      //println(classes.classesGraph);

      //RA: Parse order from a file, the file need to be rol enter rol enter ...
      var li = List[String]()
      val s = scala.io.Source.fromFile("modelos/order.txt")
      s.getLines.foreach( (line) => {
    	  println(line.trim.toUpperCase)
    	  li ::= line;
      })
      //RA: Here I put reverse, because the last element that I added was the first one in the list
      li.reverse.foreach{ r =>
	classes.getClasses.foreach { cl =>
	  		print ("CLASE: ",cl);
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
    
    ret
  }
}
