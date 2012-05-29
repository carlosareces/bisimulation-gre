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

class PositiveClassComputer(graph: GraphT[String, String],
  rolesToProbUse: Map[String, Float],
  rolesToProbDisc: Map[String, Float],
  rolesOrdenados: List[String],
  categorias: HashMap[String, String]) {
  val classes = new ClassContainer(graph);

  def compute = { //esta parte no se ejecuta ya que son todas relaciones
    val simplifier = new dlgre.formula.Simplifier(graph);
    // initialize predicates
    graph.getAllPredicates.foreach { p =>
      {
    	  val f1: Formula = new Literal(p, true);
      	  f1.categorias = new HashSet() + categorias(p);
      	  classes.addAmbos(f1, new HashSet() + f1);//esta no deberia pasar porque no tengo literales
      	  println("CASO LITERAL :S");
      }
    }
    //RA: Probabilities for roles from modelos/order.txt
    var madeChanges = true;
    //val log = new FileWriter("log-test.txt");
    var rolesToRandUse = new HashMap[String,Float]
    //val rolesToRandDisc = new HashMap[String,Float]
    val randomProb = new FileWriter("randomProbabilities.txt") ;
    //Establezco valores random para cada relacion que quedan estaticos, no cambio de criterio
    rolesOrdenados.foreach { r =>
    	//var rand1: Float = Math.abs(new Random().nextFloat());
    	//var rand2: Float = Math.abs(new Random().nextFloat());
    	rolesToRandUse(r) = Math.abs(new Random().nextFloat());//rand1;
    	//rolesToRandDisc(r) = rand2;
    }
    println("ROLES TO RAND USE: "+rolesToRandUse);
    println("PROB ROLES:        "+rolesToProbUse);
    println("ROLES ORDENADOS: "+rolesOrdenados);
    var it:Int = 0;
    
    var probMenores1:Boolean = true; //esto va a ser el resultado de una funcion que dice si todas las prob son menos que 1
    var masveces:Int = 0;
    var count: Int = 0;
    while (!classes.isAllSingletons && probMenores1){// && (masveces!=6)) {
    	madeChanges = true;
	    while (madeChanges && !classes.isAllSingletons) { //&& (iteration < maxIteration) ) {
	      it = it+1;
	      madeChanges = false;
	     // print("----------------------------- " + it);
	      //RA: Para cada relacion le asignamos un booleano diciendo si esta relacion ya se uso.
	      var rel_used = new HashSet[String]()
	      try {
	        rolesOrdenados.foreach { r =>
	          
                var categoria: String = r;//"otra";
              	try {
                 categoria = categorias(r);
              	}
              	catch {
              	  case e:Exception => ;//println(r + " " + e.toString());
              	}

               	if (!rel_used.contains(r)) {
	            var p_use: Float = 1;//si no esta en archivo le pongo 1
	            //var p_disc: Float = 1;
	            try { p_use = rolesToProbUse(r); }
	            catch { case e: Exception => println("!!!!!!!!!!!!!!!!!!!!!!!!!!rol no estaba: "+r); }
	          /*try { p_disc = rolesToProbDisc(r); }
	            catch { case e: Exception => ; }*/
	            if (rolesToRandUse(r) <= p_use) {
	              //println("ROL paso: "+ r);
	              
	              //if (r=="green" || r=="ball") count +=1;
	              //print("\tDio prob_uso: " + p_use + " mayor que rand1: " + rolesToRandUse(r) + " reviso las clases...\n");
	              classes.getClasses.foreach { cl =>
	                //print("\tClase: " + cl.toString + "\n");
	                //print("\tRelacion: " + r + " rand2:" + rolesToRandDisc(r) + "\n");
	                var f1: Formula = new Existential(r, cl.e1.formula);
	                f1.categorias = Set(categoria);
	                
	                var set2: HashSet[Formula] = new HashSet();
	                cl.e2.foreach { entry => { 
	                    val f2: Formula = new Existential(r, entry.formula);
	                    f2.categorias = Set(categoria); // Categoria de Ex-r(*) es {r}. De Ex-r1(*)^Ex-r2(*) es {r1, r2}
	                    set2 += f2;
	                  }
	                }
                    //if (rolesToRandDisc(r) <= p_disc) {
                      //print("\t\tDiscernible: (" + new Existential(r, cl.e1.formula).prettyprint);
                      //print(", " + new Existential(r, entry.formula).prettyprint + ")\n");
                    
	                //println("Se agrega " + f1.prettyprint + ", categoria " + f1.categorias);
	                if (classes.addAmbos(f1, set2)) {
                      madeChanges = true;
                      //rel_used += r;
                      //throw new Exception("break");
                    }
	              }
	            }
	            //else println(r + ", no dio la probabilidad de uso, rand: " + rolesToRandUse(r) + " Prob_uso: " + p_use); }
	          }
	        }
	        //REVISE: Agregue madeChanges=True, por si sale por aca en caso de agrego a la derecha
	      } catch { case e: Exception => println("atrapado por exception: "+ e.toString());}
	    }
	    //print("\t\tActualizo probabilidades \n");
	    //Actualizo las probabilidades en un 10%
	    rolesOrdenados.foreach { r =>
	      if ((rolesToProbUse(r)+ (10).toFloat/(100).toFloat)<(1).toFloat){
	     	rolesToProbUse(r) = rolesToProbUse(r)+ (10).toFloat/(100).toFloat;
	        //println("Actualice a : "+rolesToProbUse(r)+" "+r)
	      }
	      else{
	        rolesToProbUse(r)=(1).toFloat;
	        //println("Actualice a  1: "+r)
	      }
	      
	    }
	    var rel_used = new HashSet[String]() 
	    
	    var hay_uno:Int = 0;
	    probMenores1=false;
	    rolesOrdenados.foreach { r =>
	      if (rolesToProbUse(r)<(1).toFloat)
	    	  probMenores1=true;   
	    }
    }
    //log.close();
    //println("Cantidad que dio green ball "+count+" de "+it*2 );
	classes.getClasses;
  }

  
  def computeForJava = {
    val ret = new java.util.HashMap[String, Formula]();
    val classes = compute;

    classes.foreach { entry =>
      entry.e1.extension.scalaIterator.foreach { x =>
        ret.put(x, entry.e1.formula);
      }
    }
    ret
  }
}
