package dlgre;
import grapht._;

object IterateRandom {
  def main(args : Array[String]) : Unit = {
    val positiveMode = (args(0) == "positive");
    val iterations = Integer.parseInt(args(1));
    val warmupIterations = Integer.parseInt(args(2));
    var sumTime : Double = 0;
    
    
    Iterator.range(0,warmupIterations).foreach { x =>
      val graph : GraphT[String,String] = dlgre.generate.RandomGenerator.generate(20, 10, 4, 0.1, 0.1);
      if( positiveMode ) {
        val result = new PositiveClassComputer(graph).compute;
      } else {
        val result = new BisimulationClassesComputer(graph).compute;
      }
    }
    
    print("(");
    Iterator.range(0, iterations).foreach { x =>
      val graph = dlgre.generate.RandomGenerator.generate(args(3), args(4), args(5), args(6), args(7));
      
      val start = System.currentTimeMillis;
      if( positiveMode ) {
        val result = new PositiveClassComputer(graph).compute;
      } else {
        val result = new BisimulationClassesComputer(graph).compute;
      }
      val end = System.currentTimeMillis;

      print(":");
      sumTime = sumTime + end-start;
    }
    print(") ");
    
    println("Average runtime: " + sumTime/iterations + " ms");
    
  }
}
