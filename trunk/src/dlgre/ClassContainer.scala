package dlgre;

import scala.collection.mutable._;
import scala.util.Random;
import dlgre.formula._;
import grapht._;

class ClassContainer(graph: GraphT[String, String], debugPrint: Boolean) {

case class Entry(extension: BitSetSet[String], formula: Formula) {
	override def equals(other: Any): Boolean = {
		other.isInstanceOf[Entry] && 
		//other.asInstanceOf[Entry].formula == formula/* && 
		other.asInstanceOf[Entry].extension == extension && 
		other.asInstanceOf[Entry].formula.categorias == formula.categorias;
	}

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
classesGraph.addNode(Entry2(Entry(graph.getNodeSet(nodes), new Top()),
		new HashSet[Entry]() += Entry(graph.getNodeSet(nodes), new Top())));

def getClasses = classesGraph.getAllNodes

def doprint(x: Any): Unit =	{
	if (debugPrint)
		println(x);
}

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
def addAmbos(f1: Formula, set2: Set[Formula]) = {
	//doprint("Formula que llega a addAmbos: "+f1.prettyprint);
	//doprint(green_ball);
	val extension1 = getExtension(f1);
	val entry1 = Entry(extension1, f1);
	var entry: Entry2 = null;
	//doprint("22222Formula que llega a addAmbos: "+f1.prettyprint);
	
	val knownClasses = getClasses; // Es una lista de Entry2.
	var ret = false;

	potentiallyUninformative.clear();

	knownClasses.foreach { other =>
			
			if (other.e1.extension.size >= 1) { // No es singleton.
				val conjunction1 = new Conjunction(List(other.e1.formula, f1)).removeConjunctionsWithTop;
				conjunction1.categorias = other.e1.formula.categorias.union(f1.categorias);
				
				val newExtension1 = other.e1.extension.intersect(extension1);
				val newEntry1 = Entry(newExtension1, conjunction1);
				
				var found: Boolean = false;
				classesGraph.getAllNodes.foreach { node => if (node.e1.equals(newEntry1)) { found = true; } };
				
				if (found == false) {//si la formula no estaba, agrega del lado izq
					entry = Entry2(Entry(newExtension1, conjunction1), new HashSet[Entry]());
				
					if ( isNontrivial(newExtension1) /*&& isInformative1(newExtension1)*/ ) {

						//doprint("\t\tSe agrega (" + entry.e1 + ", { ");
						memoizedExtensions += conjunction1 -> newExtension1;
	
						set2.foreach { f2 =>
						
							val extension2 = getExtension(f2);
							val entry2 = Entry(extension2, f2);
	
							other.e2.foreach { en =>
								val conjunction2 = new Conjunction(List(en.formula, f2)).removeConjunctionsWithTop;
								val diferentes:Boolean = en.formula.categorias.intersect(f2.categorias).isEmpty;
								//doprint("$$categorias: "+f2.categorias+" / "+en.formula.categorias);

								conjunction2.categorias = en.formula.categorias.union(f2.categorias);
								val newExtension2 = en.extension.intersect(extension2);
			
								//print("\t" + conjunction2.prettyprint + ": " + newExtension2 + ":");
								//doprint("es informativa2: " + isInformative2(newExtension2));
							    //doprint("\nOTHER:::"+en);
							    //doprint("FORMULA:::"+f2.prettyprint);
							    
								/*if (extension2.size() == 1 && extension2.contains("s1"))
								{
									doprint(conjunction2);
								}*/

								if (isNontrivial(newExtension2)) {
									memoizedExtensions += conjunction2 -> newExtension2;
									if (isInformative2(newExtension2)) { 
										doprint("Agrego "+newExtension2+" / "+ conjunction2.prettyprint);
										entry.e2 += Entry(newExtension2, conjunction2);
									}//NOTA: este else lo cancele 28/05 daba un poco de overspecification
									else {
									    /*//doprint("formulas: "+conjunction2.prettyprint+" / "+en.formula.prettyprint);
										val rolesViejos: Set[String] = en.formula.roles;
										var rolesNuevos: Set[String] = f2.roles; 
										//doprint("Roles viejos: " + rolesViejos + ", roles nuevos: " + rolesNuevos);
										if ( rolesViejos.intersect(rolesNuevos).isEmpty ) { // 
											entry.e2 += Entry(newExtension2, conjunction2);
										}*/
									    /*doprint("categorias: "+conjunction2.categorias+" / "+en.formula.categorias);
									    doprint("formulas: "+conjunction2.prettyprint+" / "+en.formula.prettyprint);*/
									    
										if ( diferentes ) { //
											//doprint("Agrego "+newExtension2+" / "+ conjunction2.prettyprint);
											//entry.e2 += Entry(newExtension2, conjunction2);
										}
									}
							    }// fin if (isNontrivial(newExtension2)) 
							}//fin other.e2.foreach
						}// fin set2.foreach
						ret = addToGraph(entry) || ret;
					} else {
						//doprint("No es informativo, no se agrega " + entry.e1);
					}// fin if ( isNontrivial(newExtension1) 
				}// fin if (found == false)
			}// if (other.e1.extension.size > 1)
		}// fin knownClasses.foreach
		if (getClasses.size > maxSize) {
			maxSize = getClasses.size;
		}
		ret
}


//-------------------------------------
def addUna(f1: Formula, overspec: Boolean, admitida: Boolean) = {
	val extension1 = getExtension(f1);

	val knownClasses = getClasses; // Es una lista de Entry2.
	var ret: Boolean = false;
	potentiallyUninformative.clear();

	knownClasses.foreach { other =>
		val conjunction1 = new Conjunction(List(f1, other.e1.formula)).removeConjunctionsWithTop;
		conjunction1.categorias = other.e1.formula.categorias.union(f1.categorias);
		val newExtension1 = other.e1.extension.intersect(extension1);
		
		if (!newExtension1.isEmpty)
			doprint("Overspecification: agregando  " + conjunction1.prettyprint);
		
		// Es para q no agregue Ex-green.(T) & Ex-green.(T)
		val categoriasDistintas: Boolean = other.e1.formula.categorias.intersect(f1.categorias).isEmpty;
		var agregar = true;
		
		if (categoriasDistintas) {
			if (isNontrivial(newExtension1)) { // Si respresenta algun elemento
				val informative: Boolean = isInformative1(newExtension1);
				if (overspec || informative) { // La formula es informativa o se hace overspecification.
					if (overspec && admitida && !informative) { // Este if es para eliminar la formula vieja.
						// Si estamos overspecificando con relacion admitida, eliminamos la formula vieja.
						if ( Math.abs(new Random().nextFloat())>0) {
							getClasses.foreach { cl =>
								if (cl.e1.extension == newExtension1) {
									if (cl.e1.formula.cantidadExiste < conjunction1.cantidadExiste) {
										doprint("Overspecification: eliminamos " + cl.e1.formula.prettyprint);
										classesGraph.removeNode(cl);
									}
									else {
										agregar = false;
										doprint("Overspecification: no eliminamos " + cl.e1.formula.prettyprint);
									}
								}
							}
							doprint("Overspecification: agregamos  " + conjunction1.prettyprint);
						}
						else
						  agregar = false;
					}
					
					memoizedExtensions += conjunction1 -> newExtension1;
					if (agregar) {
						val entry: Entry = Entry(newExtension1, conjunction1);
						if (overspec && admitida && !informative)
							addToGraph(Entry2(entry, new HashSet() + entry));
						else
							ret ||= addToGraph(Entry2(entry, new HashSet() + entry));
					}
				}
			}
		}
	}
	ret
}




//-------------------------------------
private def addToGraph(entry: Entry2) = {

	val children = findMaximalSubsets(entry);

	if (!contained(entry, children)) {

		val parents = findMinimalSupersets(entry);

		//doprint("Agregamos " + entry.e1.extension + " / " + entry.e1.formula.prettyprint);
		//doprint("Hijos " + children);
		//doprint("Padres " + parents);

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
//Encuentra las clases que tienen del lado izquierdo 
private def findMaximalSubsets(entry: Entry2) = {
	val ret = new HashSet[Entry2];

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
	val ret = new HashSet[Entry2];
	classesGraph.getLeaves.foreach { l =>
	  classesGraph.foreachDFSr(l,
	      u => if (entry.e1.extension isSubsetOf u.e1.extension) { ret += u },
	      u => entry.e1.extension isSubsetOf u.e1.extension)
	}
	ret
}

private def contained(entry: Entry2, children: Iterable[Entry2]) = {
	val unionOverSubsets = graph.getNodeSet
	children.foreach { ch => unionOverSubsets.addAll(ch.e1.extension) };
	entry.e1.extension == unionOverSubsets
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
		//doprint("Checking RUS " + pu);
		val puEntry = Entry2(pu, new HashSet[Entry]());
		if (!checked.contains(pu)
			&& !uninformativeClasses.contains(pu.extension)
			&& contained(puEntry,
					classesGraph.mapOutEdges(puEntry,
							{ edge => classesGraph.getTgt(edge) }))) {
			//doprint(" -> uninformative, removing",pu);

			classesGraph.foreachInEdge(puEntry, { src =>
				potentiallyUninformative += src.e1;

				classesGraph.foreachOutEdge(puEntry, { tgt =>
					classesGraph.addEdge(src, tgt, "");
				});
			});

			//doprint("Sacamos " + puEntry.e1.extension + " / " + puEntry.e1.formula.prettyprint);
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
              doprint("\n\n\n****ERROR**** " + set + " is uninformative!!");
              doprint("graph: " + classesGraph);
            }
          }

        } */

}
