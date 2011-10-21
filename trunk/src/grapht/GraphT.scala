package grapht;

import org.jgrapht.graph._;
import scala.collection.mutable._;
import util.StringUtils.join;

class GraphT[V,E]() {
  	private val dummyEdge = new DefaultEdge();
	private val graph = new DefaultDirectedGraph[V,DefaultEdge](dummyEdge.getClass.asInstanceOf[Class[_ <: DefaultEdge]] );
        
        private val nodesToIndices = new HashMap[V,Int]
        private val nodeList = new java.util.ArrayList[V]

        private val getNodeIndex = {nodename:V => nodesToIndices(nodename)}
        private val getNodeNameForIndex = {index:Int => nodeList.get(index)}
        
        private val predicates = new HashSet[String]
        private val roles = new HashSet[E]
        
        private val nodesToPredicates = new HashMap[V,Set[String]]
        private val edgesToRoles = new HashMap[DefaultEdge,E];
        
        
        
        /*** nodes ***/
        def addNode(u : V) = graph.addVertex(u);
        def containsNode(u:V) = graph.containsVertex(u);
        def removeNode(u:V) = graph.removeVertex(u);
        
        def getAllNodes  = new JavaSetAdaptor[V](graph.vertexSet()).toList;

        
        def addPredicate(u : V, p : String) = {
          addNode(u);
          
          if( !nodesToPredicates.contains(u) ) {
            nodesToPredicates += u -> new HashSet[String];
          }
          
          nodesToPredicates.get(u).get += p;
          predicates += p;
        }

        def getAllPredicates : Set[String] = predicates;
        
        def getPredicates(u:V) = nodesToPredicates.get(u).get;
        def hasPredicate(u:V, pred:String) = {
          nodesToPredicates.get(u) match {
            case None => false
            case Some(preds) => preds.contains(pred);
          }
        }

        // OPT: store root nodes directly
        def getRoots = getAllNodes.toList.filter { u => graph.inDegreeOf(u) == 0 };
        def getLeaves = getAllNodes.toList.filter { u => graph.outDegreeOf(u) == 0 };
        
        
	/*** edges ***/
        
        def addEdge(u : V, v : V, r : E) = {
          if( !containsNode(u) ) addNode(u);
          if( !containsNode(v) ) addNode(v);
          
          val edge = graph.addEdge(u,v);

          if( edge != null ) {
            edgesToRoles += edge -> r;
            roles += r;
          }
        }
        
        def getAllRoles : Set[E] = roles;
        
        
        private def getInEdges(u : V) = graph.incomingEdgesOf(u);
        private def getOutEdges(u:V) = graph.outgoingEdgesOf(u);
        
        def getSrc(e:DefaultEdge) = graph.getEdgeSource(e);
        def getTgt(e:DefaultEdge) = graph.getEdgeTarget(e);
        
        def getAllEdges = graph.edgeSet();
        
        def hasEdge(src:V, role:String, tgt:V) = {
          val edge = graph.getEdge(src,tgt);
          
          (edge != null) && (getRole(edge) == role)
        }
        
        def getRole(edge:DefaultEdge) = edgesToRoles.get(edge).get;
        
        
        def foreachOutEdge(u:V, proc:(V => Unit)) = {
          val it = getOutEdges(u).iterator();
          
          while( it.hasNext() ) {
            proc(graph.getEdgeTarget(it.next()));
          }
        }
        
        def foreachInEdge(u:V, proc:(V => Unit)) = {
          val it = getInEdges(u).iterator();
          
          while( it.hasNext() ) {
            proc(graph.getEdgeSource(it.next()));
          }
        }

        def foreachInEdgeWithRole(u:V, proc:((V,E) => Unit)) = {
          val it = getInEdges(u).iterator();
          
          while( it.hasNext() ) {
            val edge = it.next();
            proc(graph.getEdgeSource(edge), edgesToRoles(edge));
          }
        }

        def mapOutEdges[T](u:V, fun:(DefaultEdge => T)) = {
          val ret = new ArrayBuffer[T];
          val it = getOutEdges(u).iterator();
          
          while( it.hasNext() ) {
            ret += fun(it.next());
          }
          
          ret.toList
        }
        
        def mapInEdges[T](u:V, fun:(DefaultEdge => T)) = {
          val ret = new ArrayBuffer[T];
          val it = getInEdges(u).iterator();
          
          while( it.hasNext() ) {
            ret += fun(it.next());
          }
          
          ret.toList
        }
        
        
        
        /****** search *****/
          
          def foreachDFS(start: V, proc : V => Unit, cancel: V => Boolean) = {
            _foreachDFS(start, proc, cancel, new HashSet[V]);
          }
          
          private def _foreachDFS(u:V, proc:V => Unit, cancel:V => Boolean, visited:Set[V]) : Unit = {
            if( !visited.contains(u) ) {
              visited += u;
              proc(u);
              
              if( !cancel(u) ) {
                foreachOutEdge(u, v => _foreachDFS(v, proc, cancel, visited));
              }
            }
          }
          
          
          def foreachDFSr(start:V, proc : V => Unit, cancel: V => Boolean) = {
            _foreachDFSr(start, proc, cancel, new HashSet[V]);
          }
          
          private def _foreachDFSr(u:V, proc:V => Unit, cancel:V => Boolean, visited:Set[V]) : Unit = {
            if( !visited.contains(u) ) {
              visited += u;
              proc(u);
              
              if( !cancel(u) ) {
                foreachInEdge(u, (v:V) => _foreachDFSr(v, proc, cancel, visited));
              }
            }
          }
          

	private val and = { (a:Boolean, b:  () => Boolean) => a && b() };
        private val or = { (a:Boolean, b:  () => Boolean) => a || b() };
        
        def dfs(u:V, found:V => Boolean, combine: (Boolean, () => Boolean) => Boolean) = {
          _dfs(u, found, combine, new HashSet[V]);
        }
        
        private def _dfs(u:V, found:V => Boolean, combine: (Boolean, () => Boolean) => Boolean, visited:Set[V]) :Boolean = {
          if( !visited.contains(u) ) {
            visited += u;
            
            if( found(u) ) {
              true
            } else {
              val it = getOutEdges(u).iterator();
              var ret = false;
              
              while( it.hasNext() ) {
                ret = combine(ret, () => _dfs(graph.getEdgeTarget(it.next()), found, combine, visited));
              }

              ret
            }
          } else {
            false
          }

        }
        
        def isReachable(u:V, tgt:V) = dfs(u, { v => v == tgt }, or);

        
        /*
          def _isReachable(u:V, tgt:V, visited:Set[V]) : Boolean = {
            if( !visited.contains(u) ) {
              visited += u;
              
              if( u == tgt ) {
                true
              } else {
                val it = getOutEdges(u).iterator();
                var ret = false;
                
                while( !ret && it.hasNext() ) {
                  ret = _isReachable(graph.getEdgeTarget(it.next()), tgt, visited);
                }

                ret
              }
            } else {
              false
            }
          }
          
          def isReachable(src:V, tgt:V) = _isReachable(src, tgt, new HashSet[V]);
          */
          
          /****** support for bitsets ******/
            
          private def recomputeNodeList = {
            nodeList.clear();
            nodesToIndices.clear();
            
            nodeList.addAll(graph.vertexSet());
            
            Iterator.range(0,nodeList.size).foreach { i =>
                    nodesToIndices += nodeList.get(i) -> i;
            }
          }
          
          def getNodeSet = {
            if( nodesToIndices.isEmpty ) {
              recomputeNodeList;
            }
            
            new dlgre.BitSetSet[V](nodeList.size, getNodeIndex, getNodeNameForIndex);
          }
          
          def getNodeSet(a:Collection[V]) : dlgre.BitSetSet[V] = {
            val ret = getNodeSet;
            ret.addAll(a);
            ret;
          }
          
          
          // TODO: Why does for/yield return Iterable rather than List?
          override def toString() = {
            "Nodes:\n" + 
              join( (for( val u <- getAllNodes ) yield (u + ": " + join(getPredicates(u), " "))), "\n") +
            "\n\nEdges:\n" +
              join( for (val e <- new JavaSetAdaptor(getAllEdges)) 
                      yield graph.getEdgeSource(e) + " --[" + getRole(e) + "]--> " + graph.getEdgeTarget(e),
                    "\n")
          }
        
        
        
        
        
        
        
        
        
        

        /* ** unimplemented -- and do I need them? or would iterators be sufficient?
        def getInEdges(u : E, label : String) : List[Edge] = {
          (for( val src <- edges; val pair <- src._2; val tgt <- pair._2 if tgt == u && pair._1 == label)
            yield new Edge(src._1, pair._1, tgt)).toList
        }
        
        
        def getOutEdges(u : E, label : String) : List[Edge] = {
          (for( val tgt <- edges.getOrElse(u, new HashMap).getOrElse(label, new ArrayBuffer) )
            yield new Edge(u, label, tgt)).toList
        }
        */
}
