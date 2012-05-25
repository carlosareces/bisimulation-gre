package dlgre;

import scala.collection.mutable._;

import dlgre.formula._;
import grapht._;

class ClassContainer(graph: GraphT[String, String]) {
case class Entry(extension: BitSetSet[String], formula: Formula, categorias: HashSet[String]) {
	override def equals(other: Any): Boolean =
		other.isInstanceOf[Entry] && 
		other.asInstanceOf[Entry].extension == extension && 
		other.asInstanceOf[Entry].categorias == categorias;

	override def hashCode = extension.hashCode;

	override def toString = formula.prettyprint + ": " + extension;
}

//Ahora tenemos 2 formulas (fi_O, fi_R)
case class Entry2(e1: Entry, e2: HashSet[Entry]) {
	override def equals(that: Any): Boolean =
		that.isInstanceOf[Entry2] && that.asInstanceOf[Entry2].e1.extension == e1.extension;

	//override def equals2(that: Any): Boolean =
		//that.isInstanceOf[Entry2] && that.asInstanceOf[Entry2].e2.extension == e2.extension;
	
	override def hashCode = e1.extension.hashCode;

	override def toString = {
	    //print("PRINT!!!!!!!!!!!!!!!!");
		var res: String = e1.toString() + ", { ";
		var resAnt: String = e1.toString() + ", { ";
		var last: String = "";
		
		e2.foreach { ext => 
		  resAnt = res;
 		  res += ext.toString() + " , "
 		  last = ext.toString();
 		}
		
		resAnt += last + " }";
		resAnt;
/*		var res: String = "";
		e2.foreach { ext => res += "(" + e1.toString() + ", " + ext.toString() + ")\n" }
		res;*/
	}
    def showString = {
	    //print("PRINT!!!!!!!!!!!!!!!!");
      //sacar
/*		var res: String = e1.toString() + ", { ";
		var resAnt: String = e1.toString() + ", { ";
		var last: String = "";
		
		e2.foreach { ext => 
		  resAnt = res;
 		  res += ext.toString() + " , "
 		  last = ext.toString();
 		}
		
		resAnt += last + " }";
		resAnt;*/
		var res: String = "";
		e2.foreach { ext => res += "(" + e1.toString() + ", " + ext.toString() + ")\n" }
		res;
	}

}

val classesGraph = new grapht.GraphT[Entry2, String]();
val uninformativeClasses = new HashSet[BitSetSet[String]];
val potentiallyUninformative = new Queue[Entry];

private val memoizedExtensions = new HashMap[Formula, BitSetSet[String]];

val simplifier = new Simplifier(graph);
var maxSize = 0;
val nodes = graph.getAllNodes;

//RA: When I add Top it add with probability 1
classesGraph.addNode(Entry2(Entry(graph.getNodeSet(nodes), new Top(), new HashSet()),
		new HashSet[Entry]() += Entry(graph.getNodeSet(nodes), new Top(), new HashSet())));

def getClasses = classesGraph.getAllNodes

// Da la extension (conjunto de elementos o nombres propios) de una formula. 
private def getExtension(fmla: Formula): BitSetSet[String] = {
		if (!memoizedExtensions.contains(fmla)) {
			val ext = fmla match {
			case Existential(r, sub) => {
				if (memoizedExtensions.contains(sub)) {
					val subext = memoizedExtensions.get(sub).get.asScalaCollection; // { b1, b2 }
					graph.getNodeSet(nodes.filter { u => subext.exists { v => graph.hasEdge(u, r, v) } })
				} else { fmla.extension(graph) }
			}
			case _ => fmla.extension(graph)
			}
			memoizedExtensions += fmla -> ext;
		}
		memoizedExtensions.get(fmla).get
}

// Devuelve true si cada clase representa un solo elemento (nombre propio).
def isAllSingletons = {
		getClasses.forall { cl => cl.e1.extension.size == 1 } //Solamente me fijo en formulas fi_O
}

// Devuelve true, si se agrega nueva formula. Para esto se fija si la formula
// representa un subconjunto mas chico de elementos (pero no vacio).
def addAmbos(f1: Formula, f2: Formula, categoria: String) = {
	val extension1 = getExtension(f1);
	val extension2 = getExtension(f2);
	val entry1 = Entry(extension1, f1, new HashSet() + categoria);
	val entry2 = Entry(extension2, f2, new HashSet() + categoria);
	var entry: Entry2 = null;

	//println("Entry: " + entry);
	//println("Agregar f1: " + f1.prettyprint + " f2: " + f2.prettyprint);
	/*if (uninformativeClasses.contains(extension1) && f1!=Top) // extension 1 es pq nos fijamos solamente en 1r formula
	{
		//println("La formula1 " + f1.prettyprint + " : " + extension1 + " no es informativa");
		false;
	} else if (!isNontrivial(extension1)) {
		//println("La formula1 " + f1.prettyprint  + " : " + extension1 + " no representa ningun elemento.");
		false;
	} else if (!isNontrivial(extension2)) {
		//println("La formula2 " + f2.prettyprint  + " : " + extension2 + " no representa ningun elemento.");
		false;
	} else {*/
		val knownClasses = getClasses; // Es una lista de Entry2.
		var ret = false;

		potentiallyUninformative.clear();

		knownClasses.foreach { other =>

			if (other.e1.extension.size > 1) { // No es singleton.
				val conjunction1 = new Conjunction(List(f1, other.e1.formula)).removeConjunctionsWithTop;
				val newExtension1 = other.e1.extension.intersect(extension1);
				val newCategorias1 = other.e1.categorias + categoria;
				var newNode: Boolean = true;
				
				var found: Boolean = false;
				classesGraph.getAllNodes.foreach { node => if (node.e1 == entry1) { found = true; } };
				
				if (found == false) {
					entry = Entry2(Entry(newExtension1, conjunction1, newCategorias1), new HashSet[Entry]());
				
					if ( isNontrivial(newExtension1) /*&& isInformative1(newExtension1)*/ ) {
						//println("\t\tSe agrega (" + entry.e1 + ", { ");
						memoizedExtensions += conjunction1 -> newExtension1;
	
						other.e2.foreach { en =>
							val conjunction2 = new Conjunction(List(f2, en.formula)).removeConjunctionsWithTop;
							val newExtension2 = en.extension.intersect(extension2);
							val newCategorias2 = en.categorias + categoria;
		
							//print("\t" + conjunction2.prettyprint + ": " + newExtension2 + ":");
							//println("es informativa2: " + isInformative2(newExtension2));
						    //println("\nOTHER:::"+en);
						    //println("FORMULA:::"+f2.prettyprint);
						    
						    if (isNontrivial(newExtension2)) {
								if (isInformative2(newExtension2)){ 
									memoizedExtensions += conjunction2 -> newExtension2;
									entry.e2 += Entry(newExtension2, conjunction2, newCategorias2);
								}
								else {
									memoizedExtensions += conjunction2 -> newExtension2;
									val rolesViejos: Set[String] = en.formula.roles;
									var rolesNuevos: Set[String] = f2.roles; 
									//println("Roles viejos: " + rolesViejos + ", roles nuevos: " + rolesNuevos);
									if ( rolesViejos.intersect(rolesNuevos).isEmpty ) { // 
										entry.e2 += Entry(newExtension2, conjunction2, newCategorias2);
									}
								}
						    }
						}//fin foreach
						ret = addToGraph(entry) || ret;
					} else {
						//println("No es informativo, no se agrega " + entry.e1);
					}
				}
			}
		}
		if (getClasses.size > maxSize) {
			maxSize = getClasses.size;
		}
		ret
}


//-------------------------------------
def addUna(f2: Formula, categoria: String) = {
	val extension2 = getExtension(f2);
	val entry2 = Entry(extension2, f2, new HashSet() + categoria);

	//println("Entry: " + entry);
	//println("Agregar f1: " + f1.prettyprint + " f2: " + f2.prettyprint);
	/*if (uninformativeClasses.contains(extension1) && f1!=Top) // extension 1 es pq nos fijamos solamente en 1r formula
	{
		//println("La formula1 " + f1.prettyprint + " : " + extension1 + " no es informativa");
		false;
	} else if (!isNontrivial(extension1)) {
		//println("La formula1 " + f1.prettyprint  + " : " + extension1 + " no representa ningun elemento.");
		false;
	} else if (!isNontrivial(extension2)) {
		//println("La formula2 " + f2.prettyprint  + " : " + extension2 + " no representa ningun elemento.");
		false;
	} else {*/
		val knownClasses = getClasses; // Es una lista de Entry2.
		var ret = false;
		potentiallyUninformative.clear();

		knownClasses.foreach { other =>
			other.e2.foreach { en =>
				val conjunction2 = new Conjunction(List(f2, en.formula)).removeConjunctionsWithTop;
				val newExtension2 = en.extension.intersect(extension2);
				val newCategorias2 = en.categorias + categoria;
				//print("\t" + conjunction2.prettyprint + ": " + newExtension2 + ":");
				//println("es informativa2: " + isInformative2(newExtension2));
			    //println("\nOTHER:::"+en);
			    //println("FORMULA:::"+f2.prettyprint);

			    if (isNontrivial(newExtension2)) { // Si respresenta algun elemento
					if (isInformative2(newExtension2)) { 
						memoizedExtensions += conjunction2 -> newExtension2;
						other.e2 += Entry(newExtension2, conjunction2, newCategorias2);
					}
					else {//caso no informative 
						memoizedExtensions += conjunction2 -> newExtension2;
						val rolesViejos: Set[String] = en.formula.roles;
						var rolesNuevos: Set[String] = f2.roles; 
						println("Roles viejos: " + rolesViejos + ", roles nuevos: " + rolesNuevos +" "+rolesViejos.intersect(rolesNuevos).isEmpty);
						if ( rolesViejos.intersect(rolesNuevos).isEmpty ) { // 
							other.e2 += Entry(newExtension2, conjunction2, newCategorias2);
							println("CASO AGREGUE::: "+other+ " ::: " +conjunction2);
						}
					}
			    }
			}//fin foreach
		}
		ret
}




//-------------------------------------
private def addToGraph(entry: Entry2) = {
	val children = findMaximalSubsets(entry);

	if (isInformative(entry, children)) {
		val parents = findMinimalSupersets(entry);
		classesGraph.addNode(entry);

		children.foreach { ch => classesGraph.addEdge(entry, ch, "") }
		parents.foreach { par =>
			classesGraph.addEdge(par, entry, "");
			potentiallyUninformative += par.e1;
		}

		removeUninformativeSubsets();

		true
	} else {
		uninformativeClasses += entry.e1.extension; //ver aca si esta bien...
		false
	}
}
//-----------------------------------
private def findMaximalSubsets(entry: Entry2) = {
	val ret = new HashSet[Entry2]

	                      classesGraph.getRoots.foreach { r =>
	                      if (entry.e1.extension intersects r.e1.extension) {
	                    	  classesGraph.foreachDFS(r,
	                    			  u => if (u.e1.extension isSubsetOf entry.e1.extension) { ret += u },
	                    			  u => u.e1.extension isSubsetOf entry.e1.extension)
	                      }
	}
	ret
}

private def findMinimalSupersets(entry: Entry2) = {
	val ret = new HashSet[Entry2]
	                      classesGraph.getLeaves.foreach { l =>
	                      classesGraph.foreachDFSr(l,
	                    		  u => if (entry.e1.extension isSubsetOf u.e1.extension) { ret += u },
	                    		  u => entry.e1.extension isSubsetOf u.e1.extension)
	}
	ret
}

private def isInformative(entry: Entry2, children: Iterable[Entry2]) = {
	val unionOverSubsets = graph.getNodeSet

	children.foreach { ch => unionOverSubsets.addAll(ch.e1.extension) };
	entry.e1.extension != unionOverSubsets
}

def getMaxSize = maxSize;

// Se fija si la extension de una formula tiene uno o mas elementos.
private def isNontrivial(set: BitSetSet[String]) = {
	set.size > 0
}

// Saca las formulas "subsumed".
private def removeUninformativeSubsets() = {
	val checked = new HashSet[Entry];
	var ret = false;

	potentiallyUninformative.foreach { pu =>
	//println("Checking RUS " + pu);
	val puEntry = Entry2(pu, new HashSet[Entry]());
	if (!checked.contains(pu)
			&& !uninformativeClasses.contains(pu.extension)
			&& !isInformative(puEntry,
					classesGraph.mapOutEdges(puEntry,
							{ edge => classesGraph.getTgt(edge) }))) {
		//println(" -> uninformative, removing",pu);

		classesGraph.foreachInEdge(puEntry, { src =>
		potentiallyUninformative += src.e1;

		classesGraph.foreachOutEdge(puEntry, { tgt =>
		classesGraph.addEdge(src, tgt, "");
		});
		});

		classesGraph.removeNode(puEntry);
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
private def isInformative1(set: BitSetSet[String]) = {
	val unionOverSubsets = graph.getNodeSet;

	getClasses.foreach { cl =>
		if (cl.e1.extension isSubsetOf set) {
			unionOverSubsets.addAll(cl.e1.extension);
		}
	}

	set != unionOverSubsets
}


// Devuelve true si la extension no existe en las clases y no es "subsumed" sobre las mismas.
// _should_ be deprecated, but right now this is faster than
// computing the children and then checking informativity only over those.
private def isInformative2(set: BitSetSet[String]) = {
	val unionOverSubsets = graph.getNodeSet;

	getClasses.foreach { cl =>
		cl.e2.foreach { en =>
			if (en.extension isSubsetOf set) {
				unionOverSubsets.addAll(en.extension);
			}
		}
	}

	(set != unionOverSubsets)//||(set == unionOverSubsets) 
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
