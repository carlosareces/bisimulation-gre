package dlgre;


import java.io._
import Double._
import scala.xml.parsing.ConstructingParser
import grapht._;
import scala.collection.JavaConversions;
import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import dlgre.formula._
import grapht._;
import java.util.ArrayList
import org.jgrapht.graph._;
import scala.collection.mutable._;
import util.StringUtils.join;


object Main {
  def main(args : Array[String]) : Unit = {
    val positiveMode = (args(0) == "positive");
    

    // * warmup for runtime measurements
    
    // LB: I changed (0,20 to 0,0) these line in order to remove the runtime error in RandomGenerator
  /*  Iterator.range(0,0).foreach { x =>
      val graph = dlgre.generate.RandomGenerator.generate(20, 10, 4, 0.1, 0.1);
      //println("El graph:",graph);
      if( positiveMode ) {
        val result = new PositiveClassComputer(graph).compute;
      } else {
        val result = new BisimulationClassesComputer(graph).compute;
      }
    }*/
    
    
    val graph = readGraph(args(1))
    /*if( args(1) == "random" ) {
      dlgre.generate.RandomGenerator.generate(args(2), args(3), args(4), args(5), args(6))
    } else {
      readGraph(args(1))
    }*/
    
    //println("Loaded graph: " + graph.getAllNodes.size + " nodes, " + graph.getAllEdges.size + " edges.");
    
    print("\nComputing bisimulation classes ");
    
    val start = System.currentTimeMillis;
    val simplifier = new dlgre.formula.Simplifier(graph);
    
    val rolesToProb = new HashMap[String,Double]
    var li = List[String]()
    val so = scala.io.Source.fromFile("modelos/order.txt")
    so.getLines.foreach( line => {
    var sp = line.split(" -> ");
    //RA: adding line specting first element string "->" second element(double)
    rolesToProb(sp.apply(0)) = sp.apply(1).toDouble;
    li ::= sp.apply(0);
    })
    print ("ROLES _TO _PROB: ", rolesToProb,li);   

    
    
    if( positiveMode ) {
      println("\nPositive mode");
      val result = new PositiveClassComputer(graph, li).compute;
      
      println(" done, " + (System.currentTimeMillis - start) + " ms.");
      println("\nBisimulation classes with their concepts:");
      result.foreach { entry => println(entry.formula.removeConjunctionsWithTop.prettyprint + ": " + util.StringUtils.join(entry.extension.asScalaCollection,",")) };
      // result.foreach { entry => println(entry.extension.asScalaCollection.toString + ": " + dlgre.realize.Realizer.realize(entry.formula.removeConjunctionsWithTop, "noun", "drawer")) }; 
    } else {
      println("\nBisim mode");
      print("..[max=0]");

      val result = new BisimulationClassesComputer(graph, li, rolesToProb).compute;

      println(" done, " + (System.currentTimeMillis - start) + " ms.");
      println("\nBisimulation classes with their concepts:");
      result.foreach { fmla => println(simplifier.simplify(fmla).prettyprint + ": " + util.StringUtils.join(fmla.extension(graph).asScalaCollection,",")) };
    }
    
    
    
  }
  
  
 
  
  
  private def readGraph(filename:String) = {
    val ret =  new GraphT[String, String]
    val p = ConstructingParser.fromFile(new File(filename), true)
    val doc: xml.Document = p.document

  
  
  (doc \ "individual").foreach { indiv =>
          val node = mygetattr(indiv, "id");

      (indiv \ "predicate" ).foreach { element =>
              val pred = mygetattr(element, "pred");
              ret.addPredicate(node, pred); 
      }
      
      (indiv \ "related").foreach { element =>
              val rel = mygetattr(element, "rel");
              val to = mygetattr(element, "to");
              //RA: parsing the "prob" attribute of the xml file for probability
              //val prob = mygetattr(element, "prob");
              //RA: Giving the probability like a double
              ret.addEdge(node, to, rel);
            
      }
  }
    
    ret

  }
  
  private def mygetattr(node : scala.xml.Node, attr : String) = {
    node.attribute(attr) match { 
      case Some(a) => a.last.text; 
      case _ => throw new Exception("Undefined attribute " + attr + " in node " + node); 
    }
  }
}
