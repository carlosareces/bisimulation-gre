package dlgre;

import scala.collection.mutable._;

import dlgre.formula._;
import grapht._;

class ClassContainer(graph:GraphT[String,String]) {
  case class Entry(extension:BitSetSet[String], formula:Formula) {
    override def equals(that: Any): boolean =
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
        
        
        classesGraph.addNode(Entry(graph.getNodeSet(nodes), new Top()));

        
        
        def getClasses =  classesGraph.getAllNodes
        
        private def getExtension(fmla:Formula) : BitSetSet[String] = {
          if( !memoizedExtensions.contains(fmla) ) {
            val ext = fmla match {
              case Existential(r,sub) => if( memoizedExtensions.contains(sub) ) {
		val subext = memoizedExtensions.get(sub).get.asScalaCollection;

                graph.getNodeSet(nodes.filter { u => subext.exists { v => graph.hasEdge(u,r,v) }}) 
              } else fmla.extension(graph)
              case _ => fmla.extension(graph)
            }
            
            
            memoizedExtensions += fmla -> ext;
          }
          
          memoizedExtensions.get(fmla).get
        }
         
        
        def isAllSingletons = {
          getClasses.forall { cl => cl.extension.size  == 1 }
        }
        
        def add(subset:Formula) = {
          val extension = getExtension(subset);
          val entry = Entry(extension,subset);
          
          if( classesGraph.containsNode(entry) || uninformativeClasses.contains(extension) || !isNontrivial(extension) ) {
            false
          } else {
              val knownClasses = getClasses;
              var ret = false;
              
              potentiallyUninformative.clear();
              
              knownClasses.foreach { other =>
                if( other.extension.size > 1 ) {
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
           
            children.foreach { ch => classesGraph.addEdge(entry, ch, "") }
            parents.foreach { par => 
                classesGraph.addEdge(par, entry, "");
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
        
        private def isNontrivial(set:BitSetSet[String]) = {
          set.size > 0
        }
        
       
        
        
        private def removeUninformativeSubsets() = {
          val checked = new HashSet[Entry];
          var ret = false;
          
          potentiallyUninformative.foreach { pu =>
            //println("Checking RUS " + pu);
            if( !checked.contains(pu)
                && !uninformativeClasses.contains(pu.extension)
                && ! isInformative( pu, classesGraph.mapOutEdges(pu, { edge => classesGraph.getTgt(edge) }) ) ) {
              //println(" -> uninformative, removing");
              
              classesGraph.foreachInEdge(pu, { src =>
                potentiallyUninformative += src;
                
                classesGraph.foreachOutEdge(pu, { tgt =>
                  classesGraph.addEdge(src, tgt, "");
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
          
        }
        
         
*/        
}
