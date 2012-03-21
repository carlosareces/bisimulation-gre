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
import scala.util.Random;

object Main {//1
  def main(args : Array[String]) : Unit = {//2
   
   	val positiveMode = (args(0) == "positive");
   	val maxIteration = args(5);
   	val target = args(4);
    
   	
   	val fw = new FileWriter(args(6)+"der-formula-test.txt") ;
   	val fw2 = new FileWriter(args(6)+"der-texto-test.txt") ;
   	val fw3 = new FileWriter(args(6)+"izq-texto-test.txt") ;
   	val fw4 = new FileWriter(args(6)+"izq-formula-test.txt") ;
   
    var informative:Boolean = false;
    
 
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
    
    
    val start = System.currentTimeMillis;
    val simplifier = new dlgre.formula.Simplifier(graph);
    
    val rolesToProbUse = new HashMap[String,Float]
    val rolesToProbDisc = new HashMap[String,Float]
    var rolesDesOrdenados = List[String]()
    val so = scala.io.Source.fromFile(args(2))//"modelos/prob_uso_ord/P_uso_ord-1.txt")
    var num:Int = 0;
    
    so.getLines.foreach( line => {//3
      try {
        
    	var sp = line.split("->");
    	var rol = (sp.apply(0)).trim
    	rolesToProbUse(rol) = ((sp.apply(1).trim).toFloat)
    	rolesDesOrdenados ::= rol;
    	num = num + 1;
      }	
      
      catch { case e: Exception => println("Fail input use_probabilities file\n"); }
    })//fin 3
/*-----------------*/
    var arr = new Array[Float](num);
      var i:Int = 0;
      rolesDesOrdenados.foreach(r => {
        var p_use: Float = 1;
        p_use = rolesToProbUse(r);
        arr.update(i,p_use);
        i = i+1;
      })
      scala.util.Sorting.quickSort(arr)
     
      var rolesOrdenados = List[String]()
      arr.foreach(el => {
        rolesDesOrdenados.foreach(rol => {
        	if (rolesToProbUse(rol)==el) { 
        	  rolesOrdenados ::= rol
        	}
        })
      })

/*------------------*/      
    val so2 = scala.io.Source.fromFile(args(3))//"modelos/prob_disc/prob_disc-1.txt")
    so2.getLines.foreach( line => {//4
      try {
    	var sp2 = line.split("->");
    	//RA: adding line specting first element string "->" second element(double)
    	rolesToProbDisc(sp2.apply(0).trim) = (sp2.apply(1).trim).toFloat;
      }
      catch { case e: Exception => println("Fail disc_probabilities file\n") ; }//no hace nada en caso de leer lineas vacias o con otro formato
    })//fin 4
    if( positiveMode ) {//5
      //println("Positive mode");
      var iteration: Int = 0;
      //maxIteration: Ahora se toma como parametro
      while(iteration < maxIteration.toInt) {//6
        println("---------------------------------");
        fw.write("\n---------------------------------\n");	
        fw2.write("\n---------------------------------\n");
        iteration += 1;
        
        val result = new PositiveClassComputer(graph, rolesToProbUse, rolesToProbDisc, rolesOrdenados).compute;
        println("\n-----------------------------------------------------------------------------");
        println(" done, " + (System.currentTimeMillis - start) + " ms.");
        //println("\nBisimulation classes with their concepts (positive mode):");
        if (target=="all"){//7
            println("-------------------------------------");
            result.foreach {       
              
              entry2 => entry2.e2.foreach {
            	  	fw4.write("\n" + entry2.e1.formula.removeConjunctionsWithTop.prettyprint + ": " + util.StringUtils.join(entry2.e1.extension.asScalaCollection,",")+"\n");
		            fw3.write("\n" + entry2.e1.formula.removeConjunctionsWithTop.toString + ": " + util.StringUtils.join(entry2.e1.extension.asScalaCollection,",")+"\n");
            		entry =>
            			
		                //println(entry.formula.removeConjunctionsWithTop.prettyprint + ": " + util.StringUtils.join(entry.extension.asScalaCollection,",")+"\n");
		                //println(entry.formula.removeConjunctionsWithTop.toString + ": " + util.StringUtils.join(entry.extension.asScalaCollection,",")+"\n");
		                fw.write("\n" + entry.formula.removeConjunctionsWithTop.prettyprint + ": " + util.StringUtils.join(entry.extension.asScalaCollection,",")+"\n");
		                
		                fw2.write("\n" + entry.formula.removeConjunctionsWithTop.toString + ": " + util.StringUtils.join(entry.extension.asScalaCollection,",")+"\n");
		                  
              }
		    }
                       
        } else {
            //Aca imprimo solo para el target
              result.foreach { 
            	entry2 => entry2.e2.foreach {
            		entry =>
            			if (entry.extension.asScalaCollection.contains(target)){
		                  //println(entry.formula.removeConjunctionsWithTop.prettyprint + ": " + util.StringUtils.join(entry.extension.asScalaCollection,",")+"\n");
		                  //println(entry.formula.removeConjunctionsWithTop.toString + ": " + util.StringUtils.join(entry.extension.asScalaCollection,",")+"\n");
		                  fw.write("\n" + entry.formula.removeConjunctionsWithTop.prettyprint + ": " + util.StringUtils.join(entry.extension.asScalaCollection,",")+"\n");
		                  
		                  fw2.write("\n" + entry.formula.removeConjunctionsWithTop.toString + ": " + util.StringUtils.join(entry.extension.asScalaCollection,",")+"\n");
		                }
            	}
             
              }

              result.foreach { entry2 =>
            	//entry2 => entry2.e1 {
            		//entry =>
            		if (entry2.e1.extension.asScalaCollection.contains(target)){
		                 //println(entry2.e1.formula.removeConjunctionsWithTop.prettyprint + ": " + util.StringUtils.join(entry2.e1.extension.asScalaCollection,",")+"\n");
		                 //println(entry2.e1.formula.removeConjunctionsWithTop.toString + ": " + util.StringUtils.join(entry2.e1.extension.asScalaCollection,",")+"\n");
		                 fw4.write("\n" + entry2.e1.formula.removeConjunctionsWithTop.prettyprint + ": " + util.StringUtils.join(entry2.e1.extension.asScalaCollection,",")+"\n");
		                 
		                 fw3.write("\n" + entry2.e1.formula.removeConjunctionsWithTop.toString + ": " + util.StringUtils.join(entry2.e1.extension.asScalaCollection,",")+"\n");
		            }
            	}
              }
      }// fin 6 while result.foreach { entry => println(entry.extension.asScalaCollection.toString + ": " + dlgre.realize.Realizer.realize(entry.formula.removeConjunctionsWithTop, "noun", "drawer")) };
    
    } else { //fin  positive-node
      println("\nBisim mode");
      print("..[max=0]");

      val result = new BisimulationClassesComputer(graph, List[String](), rolesToProbUse).compute;

      var tiempo = System.currentTimeMillis - start;
      if (tiempo>1000){
        var mostrar_tiempo = tiempo/1000;
        if (mostrar_tiempo>60){
          println(" done, " + mostrar_tiempo/60 + " m.");
        }
        else println(" done, " + tiempo + " s.");
      }

      else println(" done, " + tiempo + " ms.");
      println("\nBisimulation classes with their concepts:");
      result.foreach { fmla => println(simplifier.simplify(fmla).prettyprint + ": " + util.StringUtils.join(fmla.extension(graph).asScalaCollection,",")) };
    }//fin 5 else
    fw.close();
    fw2.close();
    fw3.close();
    fw4.close();

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
