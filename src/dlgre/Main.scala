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
    
    val rolesToProbUse = new HashMap[String,Float]
    val rolesToProbDisc = new HashMap[String,Float]
    var rolesOrdenados = List[String]()
    val so = scala.io.Source.fromFile(args(2))//"modelos/prob_uso_ord/P_uso_ord-1.txt")
    so.getLines.foreach( line => {
    	var sp = line.split("->");
    	//RA: adding line specting first element string "->" second element(double)
    	//Aca lo puse asi... pero deberia sacar la probabilidad del corpus, y ponderarla de 0 a 1 para que no sea tan chiquita
    	var rol = sp.apply(0)
    	rolesToProbUse(rol) = (sp.apply(1).toFloat)/(10000.toFloat);
    	rolesOrdenados ::= rol;
    })
    println ("\nROLES_TO_PROB-USO: "+ rolesToProbUse);  
    println ("\nROLES_ORDENADOS: "+rolesOrdenados);
    val so2 = scala.io.Source.fromFile(args(3))//"modelos/prob_disc/prob_disc-1.txt")
    so2.getLines.foreach( line => {
    	var sp2 = line.split("->");
    	//RA: adding line specting first element string "->" second element(double)
    	rolesToProbDisc(sp2.apply(0)) = sp2.apply(1).toFloat;
    })
    println ("ROLES _TO _PROB-DISC: "+ rolesToProbDisc);
    if( positiveMode ) {
      println("Positive mode");
      val result = new PositiveClassComputer(graph, rolesToProbUse, rolesToProbDisc, rolesOrdenados).compute;
      
      println(" done, " + (System.currentTimeMillis - start) + " ms.");
      println("\nBisimulation classes with their concepts (positive mode):");
      result.foreach { entry => println(entry.e2.formula.removeConjunctionsWithTop.prettyprint + ": " + util.StringUtils.join(entry.e2.extension.asScalaCollection,",")) };
      println("\nOld form (positive mode):");
      result.foreach { entry => println(entry.e1.formula.removeConjunctionsWithTop.prettyprint + ": " + util.StringUtils.join(entry.e2.extension.asScalaCollection,",")) };
      
      // result.foreach { entry => println(entry.extension.asScalaCollection.toString + ": " + dlgre.realize.Realizer.realize(entry.formula.removeConjunctionsWithTop, "noun", "drawer")) }; 
    } else {
      println("\nBisim mode");
      print("..[max=0]");

      val result = new BisimulationClassesComputer(graph, List[String](), rolesToProbUse).compute;

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

      //(indiv \ "predicate" ).foreach { element =>
       //       val pred = mygetattr(element, "pred");
       //       ret.addPredicate(node, pred); 
      //}
      
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
