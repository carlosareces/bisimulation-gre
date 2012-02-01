package dlgre;
import grapht._;
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
/*
object IterateRandom {
  def main(args : Array[String]) : Unit = {
    val positiveMode = (args(0) == "positive");
    val iterations = Integer.parseInt(args(1));
    val warmupIterations = Integer.parseInt(args(2));
    var sumTime : Double = 0;
    //ESTO NO DEBERIA ESTAR ACA... si no lo llamamos porque !!!!!!!!!!!!!!!
    //RA: I add this part because I don't know how to pass like parameters
    
    val rolesToProbUse = new HashMap[String,Float]
    val rolesToProbDisc = new HashMap[String,Float]
    var rolesOrdenados = List[String]()
    val so = scala.io.Source.fromFile("modelos/prob_uso_ord/P_uso_ord-1.txt")
    
    so.getLines.foreach( line => {
    	var sp = line.split("->");
    	//RA: adding line specting first element string "->" second element(double)
    	//Aca lo puse asi... pero deberia sacar la probabilidad del corpus, y ponderarla de 0 a 1 para que no sea tan chiquita
    	var rol = sp.apply(0)
    	rolesToProbUse(rol) = (sp.apply(1).toFloat)/(10000.toFloat);
    	rolesOrdenados ::= rol;
    })
    var listaRoles = List[String]()
    //val so = scala.io.Source.fromFile("modelos/order.txt")
    //so.getLines.foreach( line => {
    //var sp = line.split(" -> ");
    //RA: adding line specting first element string "->" second element(double)
    //rolesToProbUse(sp.apply(0)) = sp.apply(1).toFloat;
    //listaRoles ::= sp.apply(0);
    //})
    println("ENTRO A ITERATE RANDOM!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
    Iterator.range(0,warmupIterations).foreach { x =>
      val graph : GraphT[String,String] = dlgre.generate.RandomGenerator.generate(20, 10, 4, 0.1, 0.1);
      if( positiveMode ) {
        val result = new PositiveClassComputer(graph, rolesToProbUse, rolesToProbDisc, rolesOrdenados).compute;
      } else {
        val result = new BisimulationClassesComputer(graph, listaRoles, rolesToProbUse).compute;
      }
    }
    
    print("(");
    Iterator.range(0, iterations).foreach { x =>
      val graph = dlgre.generate.RandomGenerator.generate(args(3), args(4), args(5), args(6), args(7));
      
      val start = System.currentTimeMillis;
      if( positiveMode ) {
        val result = new PositiveClassComputer(graph, rolesToProbUse, rolesToProbDisc, rolesOrdenados).compute;
      } else {
        val result = new BisimulationClassesComputer(graph, listaRoles, rolesToProbUse).compute;
      }
      val end = System.currentTimeMillis;

      print(":");
      sumTime = sumTime + end-start;
    }
    print(") ");
    
    println("Average runtime: " + sumTime/iterations + " ms");
    
  }
}
*/