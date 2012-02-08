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

object Main {//1
  def main(args : Array[String]) : Unit = {//2
    val positiveMode = (args(0) == "positive");
    val maxIteration = args(5);
    val target = args(4);
    val fw = new FileWriter("test.txt") ;
    val fw2 = new FileWriter("texto-test.txt") ;
    
    var informative:Boolean = false;
    if (args(6)=="informative"){
      informative = true; 
    } else {
      informative = false;
    }
   //println(args.length);
   //println(informative, target, maxIteration);
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
    
    
    val graph = readGraph(args(1));
    /*if( args(1) == "random" ) {
      dlgre.generate.RandomGenerator.generate(args(2), args(3), args(4), args(5), args(6))
    } else {
      readGraph(args(1))
    }*/
    
    //println("Loaded graph: " + graph.getAllNodes.size + " nodes, " + graph.getAllEdges.size + " edges.");
    
    //print("\nComputing bisimulation classes ");
    
    val start = System.currentTimeMillis;
    val simplifier = new dlgre.formula.Simplifier(graph);
    
    val rolesToProbUse = new HashMap[String,Float]
    val rolesToProbDisc = new HashMap[String,Float]
    var rolesOrdenados = List[String]()
    val so = scala.io.Source.fromFile(args(2))//"modelos/prob_uso_ord/P_uso_ord-1.txt")
    so.getLines.foreach( line => {//3
      try {
    	var sp = line.split("->");
    	var rol = sp.apply(0)
    	rolesToProbUse(rol) = (sp.apply(1).toFloat)
    	rolesOrdenados ::= rol;
      }	
      catch { case e: Exception => println("LINEA-no tenida en cuenta: "+ line); }
    })//fin 3
    val so2 = scala.io.Source.fromFile(args(3))//"modelos/prob_disc/prob_disc-1.txt")
    so2.getLines.foreach( line => {//4
      try {
    	var sp2 = line.split("->");
    	//RA: adding line specting first element string "->" second element(double)
    	rolesToProbDisc(sp2.apply(0)) = sp2.apply(1).toFloat;
      }
      catch { case e: Exception => println("LINEA-no tenida en cuenta: "+ line); }
    })//fin 4
    if( positiveMode ) {//5
      //println("Positive mode");
      var iteration: Int = 0;
      //maxIteration: Ahora se toma como parametro
      while(iteration < maxIteration.toInt) {//6
        println("---------------------------------");
        fw.write("\n---------------------------------");
        fw2.write("\n---------------------------------");
        iteration += 1;
        //Aca le paso false, pero le deberia pasar true para que me muestre los logs
        val result = new PositiveClassComputer(graph, rolesToProbUse, rolesToProbDisc, rolesOrdenados, informative).compute;
      
        //println(" done, " + (System.currentTimeMillis - start) + " ms.");
        //println("\nBisimulation classes with their concepts (positive mode):");
        if (target=="all"){//7
            println("-------------------------------------");
            result.foreach { entry2 =>
	        	println(entry2);
	        	fw.write(entry2 + "\n");
	        	fw2.write(entry2 + "\n");
		    }
        } else {
            //Aca imprimo solo para el target
              result.foreach { 
            	entry2 => entry2.e2.foreach {
            		entry =>
            			if (entry.extension.asScalaCollection.contains(target)){
		                  println(entry.formula.removeConjunctionsWithTop.prettyprint + ": " + util.StringUtils.join(entry.extension.asScalaCollection,",")+"\n");
		                  fw.write("\n" + entry.formula.removeConjunctionsWithTop.prettyprint + ": " + util.StringUtils.join(entry.extension.asScalaCollection,",")+" ---- \n");
		                }
            	}
            }
         }//7 fin 
      }// fin 6 while result.foreach { entry => println(entry.extension.asScalaCollection.toString + ": " + dlgre.realize.Realizer.realize(entry.formula.removeConjunctionsWithTop, "noun", "drawer")) };
    } else { //fin  positive-node
      println("\nBisim mode");
      print("..[max=0]");

      val result = new BisimulationClassesComputer(graph, List[String](), rolesToProbUse).compute;

      println(" done, " + (System.currentTimeMillis - start) + " ms.");
      println("\nBisimulation classes with their concepts:");
      result.foreach { fmla => println(simplifier.simplify(fmla).prettyprint + ": " + util.StringUtils.join(fmla.extension(graph).asScalaCollection,",")) };
    }//fin 5 else
    fw.close();
    fw2.close();
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
