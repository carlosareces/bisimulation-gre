package dlgre;

import scala.collection.mutable._;

import dlgre.formula._;
import grapht._;

class ClassContainer(graph:GraphT[String,String]) {
  case class Entry(extension:BitSetSet[String], formula:Formula) {
    override def equals(that: Any): Boolean =
      that.isInstanceOf[Entry] &&  that.asInstanceOf[Entry].extension == extension;
    
    override def hashCode = extension.hashCode;
    
    override def toString = extension + ": " + formula.prettyprint;
  }
  
  val classesGraph = new grapht.GraphT[Entry,String]();
  val uninformativeClasses = new HashSet[BitSetSet[String]];
  val potentiallyUninformative = new Queue[Entry];
  
  private val memoizedExtensions = new HashMap[Formula,BitSetSet[String]];

	val simplifier = new Simplifier(graph);
        var maxSize = 0;
        val nodes = graph.getAllNodes;
        
        //RA: When I add Top it add with probability 1
        classesGraph.addNode(Entry(graph.getNodeSet(nodes), new Top()));

        
        
        def getClasses =  classesGraph.getAllNodes
        
        // Da la extension (conjunto de elementos o nombres propios) de una formula. 
        private def getExtension(fmla:Formula) : BitSetSet[String] = {
          if( !memoizedExtensions.contains(fmla) ) {
            val ext = fmla match {
              case Existential(r,sub) => {
                if( memoizedExtensions.contains(sub) ) {
                	val subext = memoizedExtensions.get(sub).get.asScalaCollection; // { b1, b2 }
                	graph.getNodeSet(nodes.filter { u => subext.exists { v => graph.hasEdge(u,r,v) }}) 
                }
                else { fmla.extension(graph) }
              }
              case _ => fmla.extension(graph)
            }
            memoizedExtensions += fmla -> ext;
          }        
          memoizedExtensions.get(fmla).get
        }
         
        
        // Devuelve true si cada clase representa un solo elemento (nombre propio).
        def isAllSingletons = {
          getClasses.forall { cl => cl.extension.size  == 1 }
        }
        

        // Devuelve true, si se agrega nueva formula. Para esto se fija si la formula
        // representa un subconjunto mas chico de elementos (pero no vacio).
        def add(subset:Formula) = {
          val extension = getExtension(subset);
          val entry = Entry(extension,subset);
          println("Formula: ", subset);
          println("Extension: ", extension);
          if( classesGraph.containsNode(entry) ) {
        	  println("La formula ya existe");
        	  false;
          }
          else if( uninformativeClasses.contains(extension) )
          {
        	  println("Hay otra formula que representa la misma extension");
        	  false;
          }
          else if ( !isNontrivial(extension) ) {
           	  println("La formula representa conjunto vacio (no representa ningun elemento).");
        	  false;
          } else {
              val knownClasses = getClasses; // Lista de (Extension, Formula).
              var ret = false;
              
              potentiallyUninformative.clear();
              
              knownClasses.foreach { other =>
                if( other.extension.size > 1 ) { // No es singleton.
                  val newExtension = other.extension.intersect(extension);
                  val conjunction = new Conjunction(List(subset,other.formula));

                  val inter = Entry(newExtension,  conjunction);
                  
                  memoizedExtensions += conjunction -> newExtension
                    
            	  if( isNontrivial(newExtension)  && isInformative(newExtension) ) {
                        if( addToGraph(inter) ) {
                          ret = true;
                        }
                  }
                }
              }
            

              if( getClasses.size > maxSize ) {
                maxSize = getClasses.size;
              }
                
              //checkInformativity;
              ret
          }
          
        }
        
        private def addToGraph(entry:Entry) = {
          val children = findMaximalSubsets(entry);
          
          
          if( isInformative(entry, children) ) {
            val parents = findMinimalSupersets(entry);
            classesGraph.addNode(entry);
           //RA: fixme!! 0.5 because I don't know right now
            children.foreach { ch => classesGraph.addEdge(entry, ch, "")}//, 0.5) }
            parents.foreach { par => 
                //RA: fixme!! 0.5
                classesGraph.addEdge(par, entry, "");//, 0.5);
                potentiallyUninformative += par;
            }
            
            removeUninformativeSubsets();

            true
          } else {
            uninformativeClasses += entry.extension;
            false
          }
        }
        
        private def findMaximalSubsets(entry:Entry) = {
          val ret = new HashSet[Entry]
          
          classesGraph.getRoots.foreach { r =>
            if( entry.extension intersects r.extension ) {
              classesGraph.foreachDFS( r,
                  u => if( u.extension isSubsetOf entry.extension ) { ret += u },
                  u => u.extension isSubsetOf entry.extension
                )
              
            }
          }
          
          
          ret
        }
        
        private def findMinimalSupersets(entry:Entry) = {
          val ret = new HashSet[Entry]
          
          classesGraph.getLeaves.foreach { l =>
              classesGraph.foreachDFSr( l,
                  u => if( entry.extension isSubsetOf u.extension ) { ret += u },
                  u => entry.extension isSubsetOf u.extension
                )
          }
          
          
          
          ret
        }
        
        private def isInformative(entry:Entry, children:Iterable[Entry]) = {
          val unionOverSubsets = graph.getNodeSet
          
          children.foreach { ch => unionOverSubsets.addAll(ch.extension) };
          entry.extension != unionOverSubsets
        }
        
        
        
        def getMaxSize = maxSize;
        
        
        // Se fija si la extension de una formula tiene uno o mas elementos.
        private def isNontrivial(set:BitSetSet[String]) = {
          set.size > 0
        }
        
       
        // Saca las formulas "subsumed".
        private def removeUninformativeSubsets() = {
          val checked = new HashSet[Entry];
          var ret = false;
          
          potentiallyUninformative.foreach { pu =>
            println("Checking RUS " + pu);
            if( !checked.contains(pu)
                && !uninformativeClasses.contains(pu.extension)
                && ! isInformative(pu, 
                                   classesGraph.mapOutEdges(pu, 
                                                            {edge => classesGraph.getTgt(edge)}) ) ) {
              println(" -> uninformative, removing",pu);
              
              classesGraph.foreachInEdge(pu, { src =>
                potentiallyUninformative += src;
                
                classesGraph.foreachOutEdge(pu, { tgt =>
                  classesGraph.addEdge(src, tgt, "");//, 0.5);
                });
              });
              
              
              classesGraph.removeNode(pu);
              uninformativeClasses += pu.extension;
              ret = true;
            } else {
              checked += pu;
            }
          }
          
          potentiallyUninformative.clear;
          
          ret;
        }
        
        
        // Devuelve true si la extension no existe en las clases y no es "subsumed" sobre las mismas.
        // _should_ be deprecated, but right now this is faster than
        // computing the children and then checking informativity only over those.
          private def isInformative(set:BitSetSet[String]) = {
            val unionOverSubsets = graph.getNodeSet;
  
            getClasses.foreach { cl =>
              if( cl.extension isSubsetOf set ) {
                 unionOverSubsets.addAll(cl.extension);       
              }
            }
  
            set != unionOverSubsets
        }
        
        /*
        private def checkInformativity = {
          val unionOverSubsets = graph.getNodeSet;
          
          getClassesIterator.foreach { set =>
            unionOverSubsets.clear();
            
            getClassesIterator.foreach { cl =>
              if( (set != cl) && (cl.extension isSubsetOf set.extension) ) {
                unionOverSubsets.addAll(cl.extension);       
              }
            }
            
            if( set.extension == unionOverSubsets ) {
              println("\n\n\n****ERROR**** " + set + " is uninformative!!");
              println("graph: " + classesGraph);
            }
          }
          
        } */
          
}
