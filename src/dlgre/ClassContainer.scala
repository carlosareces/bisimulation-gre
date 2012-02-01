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
 
  //Ahora tenemos 2 formulas (fi_O, fi_R)
  case class Entry2(e1:Entry, e2:Entry){
    override def equals(that: Any): Boolean =
      that.isInstanceOf[Entry2] &&  that.asInstanceOf[Entry2].e1.extension == e1.extension && 
      							    that.asInstanceOf[Entry2].e2.extension == e2.extension;
    
    override def hashCode = e2.extension.hashCode;
    
    override def toString = "(" + e1.toString() + ", " + e2.toString() + ")";
  }
  
  val classesGraph = new grapht.GraphT[Entry2,String]();
  val uninformativeClasses = new HashSet[BitSetSet[String]];
  val potentiallyUninformative = new Queue[Entry2];
  
  private val memoizedExtensions = new HashMap[Formula,BitSetSet[String]];

	val simplifier = new Simplifier(graph);
        var maxSize = 0;
        val nodes = graph.getAllNodes;
        
        //RA: When I add Top it add with probability 1
        classesGraph.addNode( Entry2( Entry( graph.getNodeSet(nodes), new Top() ),
                                      Entry( graph.getNodeSet(nodes), new Top() ) ) );

        
        
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
          getClasses.forall { cl => cl.e2.extension.size  == 1 }//Solamente me fijo en formulas fi_R
        }
        

        // Devuelve true, si se agrega nueva formula. Para esto se fija si la formula
        // representa un subconjunto mas chico de elementos (pero no vacio).
        def add(f1:Formula, f2:Formula ) = {
          val extension1 = getExtension(f1);
          val extension2 = getExtension(f2);
          val entry1 = Entry(extension1,f1);
          val entry2 = Entry(extension2,f2);
          val entry = Entry2(entry1,entry2);
          //println("Entry: " + entry);
          if( classesGraph.containsNode(entry) ) {
        	  //println("La formula ya existe");
        	  false;
          }
          else if( uninformativeClasses.contains(extension1) )
          {
        	  //println("Hay otra formula que representa la misma extension");
        	  false;
          }
          else if ( !isNontrivial(extension1) ) {
           	  //println("La formula representa conjunto vacio (no representa ningun elemento).");
        	  false;
          } else {
              val knownClasses = getClasses; // Lista de (Entry, Entry).
              var ret = false;
              
              potentiallyUninformative.clear();
              
              knownClasses.foreach { other =>
                if( other.e1.extension.size > 1 ) { // No es singleton.
                  val newExtension1 = other.e1.extension.intersect(extension1);
                  val newExtension2 = other.e2.extension.intersect(extension2);
                  val conjunction1 = new Conjunction(List(f1,other.e1.formula));
                  val conjunction2 = new Conjunction(List(f2,other.e2.formula));

                  
                  
                  memoizedExtensions += conjunction1 -> newExtension1
                  memoizedExtensions += conjunction2 -> newExtension2  
                  
            	  if( isNontrivial(newExtension1)  && isInformative(newExtension1) ) {
                      val inter = Entry2(Entry(newExtension1,  conjunction1),Entry(newExtension2,  conjunction2));
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
        
        private def addToGraph(entry:Entry2) = {
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
            uninformativeClasses += entry.e1.extension;
            false
          }
        }
        
        private def findMaximalSubsets(entry:Entry2) = {
          val ret = new HashSet[Entry2]
          
          classesGraph.getRoots.foreach { r =>
            if( entry.e1.extension intersects r.e1.extension ) {
              classesGraph.foreachDFS( r,
                  u => if( u.e1.extension isSubsetOf entry.e1.extension ) { ret += u },
                  u => u.e1.extension isSubsetOf entry.e1.extension
                )
              
            }
          }
          
          
          ret
        }
        
        private def findMinimalSupersets(entry:Entry2) = {
          val ret = new HashSet[Entry2]
          
          classesGraph.getLeaves.foreach { l =>
              classesGraph.foreachDFSr( l,
                  u => if( entry.e1.extension isSubsetOf u.e1.extension ) { ret += u },
                  u => entry.e1.extension isSubsetOf u.e1.extension
                )
          }
          
          
          
          ret
        }
        
        private def isInformative(entry:Entry2, children:Iterable[Entry2]) = {
          val unionOverSubsets = graph.getNodeSet
          
          children.foreach { ch => unionOverSubsets.addAll(ch.e1.extension) };
          entry.e1.extension != unionOverSubsets
        }
        
        
        
        def getMaxSize = maxSize;
        
        
        // Se fija si la extension de una formula tiene uno o mas elementos.
        private def isNontrivial(set:BitSetSet[String]) = {
          set.size > 0
        }
        
       
        // Saca las formulas "subsumed".
        private def removeUninformativeSubsets() = {
          val checked = new HashSet[Entry2];
          var ret = false;
          
          potentiallyUninformative.foreach { pu =>
            //println("Checking RUS " + pu);
            if( !checked.contains(pu)
                && !uninformativeClasses.contains(pu.e1.extension)
                && ! isInformative(pu, 
                                   classesGraph.mapOutEdges(pu, 
                                                            {edge => classesGraph.getTgt(edge)}) ) ) {
              //println(" -> uninformative, removing",pu);
              
              classesGraph.foreachInEdge(pu, { src =>
                potentiallyUninformative += src;
                
                classesGraph.foreachOutEdge(pu, { tgt =>
                  classesGraph.addEdge(src, tgt, "");//, 0.5);
                });
              });
              
              
              classesGraph.removeNode(pu);
              uninformativeClasses += pu.e1.extension;
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
              if( cl.e1.extension isSubsetOf set ) {
                 unionOverSubsets.addAll(cl.e1.extension);       
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
