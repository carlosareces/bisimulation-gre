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

object IterateRandom {
  def main(args : Array[String]) : Unit = {
    val positiveMode = (args(0) == "positive");
    val iterations = Integer.parseInt(args(1));
    val warmupIterations = Integer.parseInt(args(2));
    var sumTime : Double = 0;
    
    //RA: I add this part because I don't know how to pass like parameters
    val rolesToProbUse = new HashMap[String,Float]
    val rolesToProbDisc = new HashMap[String,Float]
    
    var listaRoles = List[String]()
    val so = scala.io.Source.fromFile("modelos/order.txt")
    so.getLines.foreach( line => {
    var sp = line.split(" -> ");
    //RA: adding line specting first element string "->" second element(double)
    rolesToProbUse(sp.apply(0)) = sp.apply(1).toFloat;
    listaRoles ::= sp.apply(0);
    })
    
    Iterator.range(0,warmupIterations).foreach { x =>
      val graph : GraphT[String,String] = dlgre.generate.RandomGenerator.generate(20, 10, 4, 0.1, 0.1);
      if( positiveMode ) {
        val result = new PositiveClassComputer(graph, rolesToProbUse, rolesToProbDisc).compute;
      } else {
        val result = new BisimulationClassesComputer(graph, listaRoles, rolesToProbUse).compute;
      }
    }
    
    print("(");
    Iterator.range(0, iterations).foreach { x =>
      val graph = dlgre.generate.RandomGenerator.generate(args(3), args(4), args(5), args(6), args(7));
      
      val start = System.currentTimeMillis;
      if( positiveMode ) {
        val result = new PositiveClassComputer(graph, rolesToProbUse, rolesToProbDisc).compute;
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
