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
  rolesOrdenados: List[String]) {
  val classes = new ClassContainer(graph);

  def compute = { //esta parte no se ejecuta ya que son todas relaciones
    val simplifier = new dlgre.formula.Simplifier(graph);
    // initialize predicates
    graph.getAllPredicates.foreach { p =>
      classes.addAmbos(new Literal(p, true), new Literal(p, true));//esta no deberia pasar porque no tengo literales
      println("CASO LITERAL :S");
    }
    //RA: Probabilities for roles from modelos/order.txt

    var madeChanges = true;

    val log = new FileWriter("log-test.txt");
    val rolesToRandUse = new HashMap[String,Float]
    val rolesToRandDisc = new HashMap[String,Float]
    
    //Establezco valores random para cada relacion que quedan estaticos, no cambio de criterio
    rolesOrdenados.foreach { r =>
    	var rand1: Float = Math.abs(new Random().nextFloat());
    	var rand2: Float = Math.abs(new Random().nextFloat());
    	rolesToRandUse(r) = rand1;
    	rolesToRandDisc(r) = rand2;
    }
    var it:Int = 0;
    while (madeChanges && !classes.isAllSingletons) { //&& (iteration < maxIteration) ) {
      //print("En while mientras haya cambios y no sean todos singletones------------------\n\n");
      it = it+1;
      madeChanges = false;
      //print("----------------------------- " + iteration);

      //RA: Para cada relacion le asignamos un booleano diciendo si esta relacion ya se uso.
      var rel_used = new HashSet[String]()

      try {
        //println("ROLES ORDENADOS: "+rolesOrdenados)
        rolesOrdenados.foreach { r =>
          //print("ROL: " + r + "\n");
          if (!rel_used.contains(r)) {

            var p_use: Float = 1;
            var p_disc: Float = 1;

            try { p_use = rolesToProbUse(r); }
            catch {
              case e: Exception =>
            }

            try { p_disc = rolesToProbDisc(r); }
            catch {
              case e: Exception =>
            }
            if (rolesToRandUse(r) <= p_use) {
              //print("\tDio prob_uso: " + p_use + " mayor que rand1: " + rolesToRandUse(r) + " reviso las clases...\n");
              classes.getClasses.foreach { cl =>

                print("\tClase: " + cl.toString + "\n");
                print("\tRelacion: " + r + " P_disc: " + p_disc + " rand2:" + rolesToRandDisc(r) + "\n");

                cl.e2.foreach { entry =>
                  if (rolesToRandDisc(r) <= p_disc) {
                    //print("\t\tDiscernible: (" + new Existential(r, cl.e1.formula).prettyprint);
                    //print(", " + new Existential(r, entry.formula).prettyprint + ")\n");
                	
                    if (classes.addAmbos((new Existential(r, cl.e1.formula)), new Existential(r, entry.formula))) {
                      madeChanges = true;
                      rel_used += r;
                      
                      //throw new Exception("break");//para que era?
                    }
                  } else if (classes.addUna(new Existential(r, entry.formula))){
                		  rel_used += r;
                		  madeChanges=true;
                		  //throw new Exception("break"); //para que era? un break,salir del  ciclo try
                  }
                }
              }
            } else {
              println(r + ", no dio la probabilidad de uso, rand: " + rolesToRandUse(r) + " Prob_uso: " + p_use);
            }
          }
        }
        //REVISE: Agregue madeChanges=True, por si sale por aca en caso de agrego a la derecha
      } catch { case e: Exception => }
    }
    //print("\t\tClases que quedaron: " + classes.getClasses);
    log.close();
    classes.getClasses

  }

  def computeForJava = {//que es esto, porque es de e1??
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
