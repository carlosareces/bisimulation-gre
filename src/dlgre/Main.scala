package dlgre;


import java.io._
import Double._
import scala.xml.parsing.ConstructingParser
import grapht._
import scala.collection.JavaConversions
import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import dlgre.formula._
import grapht._
import java.util.ArrayList
import org.jgrapht.graph._
import scala.collection.mutable._
import util.StringUtils.join
import scala.util.Random;
import com.sun.org.apache.xalan.internal.xsltc.compiler.ForEach


object Main {//1
  def covers(clase1 : String, clase2 : String, targets : String) : Boolean = {
	  var answer = true;
	  targets.split(" ").foreach( x => if (!(clase1 + clase2).contains(x)) {answer = false;})
	  (clase1 + clase2).split(" ").foreach( x =>  if ( !targets.contains(x)) {answer = false;})
	  answer
  }
  def main(args : Array[String]) : Unit = {//2
	//args.foreach(r => {
	//	println(r);
	//});
	
	val current = new java.io.File( "." ).getCanonicalPath();
   // println("Current dir: " + current);

   	val positiveMode = (args(0) == "positive");
    val maxIteration = args(4);
   	val target = args(3);
   	
   	var targetAll:Boolean=(args(3) =="all");
   	val targets:HashSet[String]= new HashSet[String];
   	val collectiveMode = (args(7) == "plural");

   	if( !targetAll){
	    var sp_target = target.split(",");
	    sp_target.foreach( tar => {//3
	    	targets += tar.trim;
	    	//println (tar);
	    })
   	}
  	
   	
   	
   	
   	//este archivo va a tener las palabras permitidas para overspecificacion
   	val cat = scala.io.Source.fromFile(args(5));
   	//aca reemplace 6 por 5 es el directorio donde se ponen los resultados
   	val fw = new FileWriter(args(6)+"formula.txt");
   	val fw2 = new FileWriter(args(6)+"texto.txt");
    var informative:Boolean = false;

    val graph = readGraph(args(1));
    
    var categoriasAdmitidas:Set[String]= new HashSet[String];//("ball", "cube", "green", "blue", "large", "centre", "front", "small", "left", "right","top");
    
    cat.getLines.foreach( line => {//3
    	categoriasAdmitidas += line.trim;
    })
    
    
    
    val start = System.currentTimeMillis;
    val simplifier = new dlgre.formula.Simplifier(graph);
    
    val rolesToProbUse = new HashMap[String,Float];
    val rolesToProbUseFijos = new HashMap[String,Float];
    val rolesToProbDisc = new HashMap[String,Float]; //lo dejo por si en el futuro lo usamos por ahora va a estar vacio
    var rolesDesOrdenados = List[String]();
    val so = scala.io.Source.fromFile(args(2));//"modelos/prob_uso_ord/P_uso_ord-1.txt")
    var num:Int = 0;
    val categorias = new HashMap[String, String];
    
    so.getLines.foreach( line => {//3
      try {
    	var sp = line.split("->");
    	
    	var rol = (sp.apply(0)).trim
    	var valor = ((sp.apply(1).trim).toFloat);
    	rolesToProbUse(rol) = valor;
    	rolesToProbUseFijos(rol) = valor;
    	rolesDesOrdenados ::= rol;
    	num = num + 1;
      }	
      
      catch { case e: Exception => println("Fail input "+args(2) +", " + e + line); }
    })//fin 3
    
    cat.getLines.foreach( line => {//3
      try {
        
    	var sp = line.split(":");
    	var c = (sp.apply(0)).trim;
    	var lista = (sp.apply(1)).trim;
    	var parte = lista.split(" ");
    	parte.foreach( elem => {categorias += elem.trim -> c})
       }	
     catch { case e: Exception => println("Fail input categories file\n"+e); }
    })//fin 3
    //print("Categorias:"+categorias);

/*-----------------*/
	type Pair = (String, Float);
	val arr = new Array[Pair](num);
	var i: Int = 0;
	rolesDesOrdenados.foreach(r => {
		var p_use: Float = 1;
		p_use = rolesToProbUse(r);
		arr.update(i,(r, p_use));
		i = i+1;
	});
	
	scala.util.Sorting.quickSort(arr)(new Ordering[Pair] {
		def compare(x: Pair, y: Pair) = {
			x._2 compare y._2
		}
	});

	var rolesOrdenados = List[String]();
	arr.foreach(el => {
		rolesOrdenados ::= el._1;
	})

/*------------------*/      
    /*aca comente la parte de levantar archivo de discernibilidades.
     * val so2 = scala.io.Source.fromFile(args(3))//"modelos/prob_disc/prob_disc-1.txt")
    so2.getLines.foreach( line => {//4
      try {
    	var sp2 = line.split("->");
    	//RA: adding line specting first element string "->" second element(double)
    	rolesToProbDisc(sp2.apply(0).trim) = (sp2.apply(1).trim).toFloat;
      }
      catch { case e: Exception => println("Fail disc_probabilities file\n") ; }//no hace nada en caso de leer lineas vacias o con otro formato
    })//fin 4*/
    //print(graph)
    
    if( positiveMode ) {//5
      //println("Positive mode");
      var iteration: Int = 0;
      //maxIteration: Ahora se toma como parametro
      while(iteration < maxIteration.toInt) {//6
        //println("---------------------------------");
        fw.write("\n---------------------------------"+iteration+"\n");	
        fw2.write("\n---------------------------------"+iteration+"\n");
        iteration += 1;
        if (iteration!=0) {
        	rolesToProbUseFijos.foreach(r => {
        	rolesToProbUse+= r;
        	})
        }
        val debugPrint = false;
        val result = new PositiveClassComputer(args(6),iteration, graph, rolesToProbUse, /*rolesToProbDisc,*/ rolesOrdenados, categorias, categoriasAdmitidas, targets, targetAll, debugPrint, collectiveMode).compute;
        var toptop = ""
	    result.foreach { 
			entry2 => entry2.e2.foreach {
				entry => if (util.StringUtils.join(entry.extension.asScalaCollection, " ").length > toptop.length) {toptop = util.StringUtils.join(entry.extension.asScalaCollection, " ")}
			}
	    }
        
        //println("\n-----------------------------------------------------------------------------"+iteration+"\n");
        //println(" done, " + (System.currentTimeMillis - start) + " ms.");
        //println("\nBisimulation classes with their concepts (positive mode):");
        if (target=="all"){//7
            //println("-------------------------------------");
            result.foreach {       
              
              entry2 => entry2.e2.foreach {
            	  	//fw4.write("\n" + entry2.e1.formula.removeConjunctionsWithTop.prettyprint + ": " + util.StringUtils.join(entry2.e1.extension.asScalaCollection,",")+"\n");
		            //fw3.write("\n" + entry2.e1.formula.removeConjunctionsWithTop.toString + ": " + util.StringUtils.join(entry2.e1.extension.asScalaCollection,",")+"\n");
            		entry =>
            			
		                //println(entry.formula.removeConjunctionsWithTop.prettyprint + ": " + util.StringUtils.join(entry.extension.asScalaCollection,",")+"\n");
		                //println(entry.formula.removeConjunctionsWithTop.toString + ": " + util.StringUtils.join(entry.extension.asScalaCollection,",")+"\n");
		                fw.write("\n" + entry.formula.removeConjunctionsWithTop.prettyprint + ": " + util.StringUtils.join(entry.extension.asScalaCollection,",")+"\n");
		                
		                fw2.write("\n" + entry.formula.removeConjunctionsWithTop.toString + ": " + util.StringUtils.join(entry.extension.asScalaCollection,",")+"\n");
		                  
              }
		    }
                       
        } else {
            //Aca imprimo solo para el target
        	  var found = false;
        	  val aux = result;
              result.foreach { 
            	entry2 => entry2.e2.foreach {
            		entry =>
            		  aux.foreach {
            		    x => x.e2.foreach {
            		      val clase2 = util.StringUtils.join(entry.extension.asScalaCollection, " ")
            		      val tars = targets.mkString(" ")
            		      y => val clase1 = util.StringUtils.join(y.extension.asScalaCollection, " "); if (covers(clase1,clase2,tars) && clase1 != toptop && clase2 != toptop) {found = true;}  //tenemos que chequear entre todas las clases si puedo generar el target 
            		    		  // como la union de clases, para cada par (y,entry), tengo que ver si cubren el target
            		    }
            		  }
            			//if (entry.extension.asScalaCollection.contains(target)){
		                  //println(entry.formula.removeConjunctionsWithTop.prettyprint + ": " + util.StringUtils.join(entry.extension.asScalaCollection,",")+"\n");
		                  println(entry.formula.removeConjunctionsWithTop.toString + ": " + util.StringUtils.join(entry.extension.asScalaCollection,",")+"\n");
		                  fw.write("\n" + entry.formula.removeConjunctionsWithTop.prettyprint + ": " + util.StringUtils.join(entry.extension.asScalaCollection,",")+"\n");
		                  
		                  fw2.write("\n" + entry.formula.removeConjunctionsWithTop.toString() + ": " + util.StringUtils.join(entry.extension.asScalaCollection,",")+"\n");
		                //}
            		}
             
              	}

              //result.foreach { entry2 =>
            	//entry2 => entry2.e1 {
            		//entry =>
            		//if (entry2.e1.extension.asScalaCollection.contains(target)){
		                 //println(entry2.e1.formula.removeConjunctionsWithTop.prettyprint + ": " + util.StringUtils.join(entry2.e1.extension.asScalaCollection,",")+"\n");
		                 //println(entry2.e1.formula.removeConjunctionsWithTop.toString + ": " + util.StringUtils.join(entry2.e1.extension.asScalaCollection,",")+"\n");
		              //   fw4.write("\n" + entry2.e1.formula.removeConjunctionsWithTop.prettyprint + ": " + util.StringUtils.join(entry2.e1.extension.asScalaCollection,",")+"\n");
		                 
		                // fw3.write("\n" + entry2.e1.formula.removeConjunctionsWithTop.toString + ": " + util.StringUtils.join(entry2.e1.extension.asScalaCollection,",")+"\n");
		            //}
            	//}
              }
      }// fin 6 while result.foreach { entry => println(entry.extension.asScalaCollection.toString + ": " + dlgre.realize.Realizer.realize(entry.formula.removeConjunctionsWithTop, "noun", "drawer")) };
    
    } else { //fin  positive-node
	      println("\nBisim mode");
	      print("..[max=0]");
    }
    val result = new BisimulationClassesComputer(graph, List[String](), rolesToProbUse).compute;

    var tiempo = System.currentTimeMillis - start;
   /* println("iglesia->"+Math.abs(new Random().nextFloat()));
    println("in->"+Math.abs(new Random().nextFloat()));
    println("CalleDeSantoTomas->"+Math.abs(new Random().nextFloat()));
    println("MinisterioAsuntosExteriores->"+Math.abs(new Random().nextFloat()));
    println("frente->"+Math.abs(new Random().nextFloat()));
    println("CalleDeAtocha->"+Math.abs(new Random().nextFloat()));
    println("cerca->"+Math.abs(new Random().nextFloat()));
    */

      //println("\nBisimulation classes with their concepts:");
      //result.foreach { fmla => println(simplifier.simplify(fmla).prettyprint + ": " + util.StringUtils.join(fmla.extension(graph).asScalaCollection,",")) };

    //fin 5 else
   /*
    
    println("large->"+Math.abs(new Random().nextFloat()));
    println("ball->"+Math.abs(new Random().nextFloat()));
    println("centre->"+Math.abs(new Random().nextFloat()));
    println("front->"+Math.abs(new Random().nextFloat()));
    println("left-of->"+Math.abs(new Random().nextFloat()));
    println("cube->"+Math.abs(new Random().nextFloat()));
    println("green->"+Math.abs(new Random().nextFloat()));
    println("blue->"+Math.abs(new Random().nextFloat()));
    println("small->"+Math.abs(new Random().nextFloat()));
    println("right-of->"+Math.abs(new Random().nextFloat()));
    println("on-top->"+Math.abs(new Random().nextFloat()));
    println("above-of->"+Math.abs(new Random().nextFloat()));
    println("right>"+Math.abs(new Random().nextFloat()));
    println("left->"+Math.abs(new Random().nextFloat()));
    println("top->"+Math.abs(new Random().nextFloat()));*/
   
    fw.close();
    fw2.close();
 

    

  }

  
  
  private def readGraph(filename:String) = {
    val ret =  new GraphT[String, String]
    val p = ConstructingParser.fromFile(new File(filename), true)
    val doc: xml.Document = p.document
    //(p.document \ "individual").foreach { indiv =>
   //modifique ESTO:30-12
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
     //modifique esto 30-12
      node.attribute(attr) match {
      //scala.xml.Node.attribute(attr) match { 
      case Some(a) => a.last.text; 
      case _ => throw new Exception("Undefined attribute " + attr + " in node " + node); 
    }
  }
  
}
