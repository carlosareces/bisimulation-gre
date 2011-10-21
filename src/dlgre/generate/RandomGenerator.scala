package dlgre.generate;

import dlgre._;
import grapht._;

object RandomGenerator {
        def generate(numNodes:String, numPreds:String, numRoles:String, predDensity:String, edgeDensity:String) : GraphT[String,String] = {
          generate(Integer.parseInt(numNodes), Integer.parseInt(numPreds), Integer.parseInt(numRoles),
              java.lang.Double.parseDouble(predDensity), java.lang.Double.parseDouble(edgeDensity))
        }

        def generate(numNodes:Int, numPreds:Int, numRoles:Int, predDensity:Double, edgeDensity:Double) = {
   		val ret = new GraphT[String,String]();
                val rand = new Random();
                
                Iterator.range(1, numNodes) foreach { i =>
                  ret.addNode("u" + i);
                  
                  Iterator.range(1, numPreds) foreach { j =>
                    if( rand.nextDouble <= predDensity ) {
                      ret.addPredicate("u" + i, "p" + j);
                    }
                  }
                }
                
                Iterator.range(1, numNodes) foreach { i =>
                  Iterator.range(1, numNodes) foreach { j =>
                    Iterator.range(1, numRoles) foreach { k =>
                      if( rand.nextDouble <= edgeDensity ) {
                        ret.addEdge("u" + i, "u" + j, "e" + k);
                      }
                    }
                  }
                }
                
                ret;
        }
}
