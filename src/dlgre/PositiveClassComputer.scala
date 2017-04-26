package dlgre;

import scala.collection.mutable.Queue
import java.io._
import scala.collection.mutable.Set
import dlgre.formula._
import grapht._;
import java.util.ArrayList

import org.jgrapht.graph._;
import scala.collection.mutable._;
import scala.util.Random;
import util.StringUtils.join;

import collection.JavaConversions._;

class PositiveClassComputer(carpeta: String,iteration:Int, graph: GraphT[String, String],
		rolesToProbUseOrig: Map[String, Float],
		/*rolesToProbDisc: Map[String, Float],*/
		rolesOrdenados: List[String],
		categorias: HashMap[String, String],
		categoriasAdmitidas: Set[String],
		targets: HashSet[String],
		targetAll: Boolean,
		debugPrint: Boolean,
		collectiveMode: Boolean) {
	val classes = new ClassContainer(graph, debugPrint);


	def doprint(x: Any): Unit =	{
		if (debugPrint)
			println(x);
	}

	def hasTar(formula : String, targets : HashSet[String]) : Boolean = {
	  var ans : Boolean = false;
	  var values = targets.toList;
	  //println("Targets = " + values);
	  //println("formula = " + formula);
	  values.foreach{ tar => if (formula.contains(tar)) {ans = true;}}
	  //println("hasTar devuelve + " + ans);
	  ans
	}

	def isTar(formula : String, targets : HashSet[String]) : Boolean = {
	  var ans : Boolean = true;
	  var values = targets.toList;
	  //println("Targets = " + values);
	  //println("formula = " + formula);
	  values.foreach{ tar => if (formula.contains(tar)) {ans = true && ans;} else {ans = false;}}
	  //println("isTar devuelve + " + ans);
	  ans
	}


	def compute = {
		//esta parte no se ejecuta ya que son todas relaciones
		val simplifier = new dlgre.formula.Simplifier(graph);
		// initialize predicates
		graph.getAllPredicates.foreach {
			p => {
				val f1: Formula = new Literal(p, true);
				f1.categorias = new HashSet() + categorias(p);
				classes.addAmbos(f1, new HashSet() + f1);//esta no deberia pasar porque no tengo literales
			}
		}

		//RA: Probabilities for roles from modelos/order.txt
		var rolesToRandUse = new HashMap[String,Float];
		var rolesToSaltitosProb = new HashMap[String,Float];
		//val rolesToRandDisc = new HashMap[String,Float]
		//var file_random_prob = new FileWriter(carpeta+"randomProbabilities.txt", true) ;
		//Establezco valores random para cada relacion que quedan estaticos, no cambio de criterio
		//file_random_prob.write("\n---------------------------------vez: "+iteration+"\n");
		var rolesToProbUse = rolesToProbUseOrig;
		rolesOrdenados.foreach { r =>
			rolesToRandUse(r) = Math.abs(new Random().nextFloat());//rand1;
			rolesToSaltitosProb(r) = (1.0f - rolesToProbUse(r)) / 10.0f;  
			//file_random_prob.write(r+"->"+rolesToRandUse(r)+"\n");
		}
		var agregoAlgo = true;
		var termino = false;
		var todasProb1:Boolean = false; //todas las prob son igual a 1
		var over:Boolean = true;
		var allSingletons: Boolean = false;
		//agregue esto
		var isTarget: Boolean = false;
		
		while (!termino) {

			if (!agregoAlgo) { 
				todasProb1 = true;
				rolesOrdenados.foreach { r =>
					if (rolesToProbUse(r) < 1.0f) {
						todasProb1=false;   
					}
				}
			}

			agregoAlgo = false;
			try {
				rolesOrdenados.foreach { r =>

				var categoria: String = r;
				try {
					categoria = categorias(r);
				}
				catch {
					case e:Exception => ;
				}
				
				var p_use: Float = 0;//si no esta en archivo le pongo 0, tiene menos que todas las que si estan!
				//var p_disc: Float = 1;
				try { p_use = rolesToProbUse(r); }
				catch { case e: Exception => ;}
				/*try { p_disc = rolesToProbDisc(r); }
        		catch { case e: Exception => ; }*/
				if (rolesToRandUse(r) <= p_use) {
					if (termino)
						doprint("termino con relacion " + r);
					
					classes.getClasses.foreach {
						cl => {
							var f1: Formula = new Existential(r, cl.e1.formula);
							
							if (collectiveMode) {

								val hassome : Boolean = hasTar(f1.extension(graph).toString, targets);
								
								// Veamos ahora si es el target mismo
								val hasall : Boolean = isTar(f1.extension(graph).toString, targets);
								
								var condition : Boolean = !hassome || hasall;
								//val condition : Boolean = true;
								if (condition) {
									var set2: HashSet[Formula] = new HashSet();
									cl.e2.foreach { 
										entry => { 
											val f2: Formula = new Existential(r, entry.formula);
											set2 += f2;
										}
									}
									
									val admitido: Boolean = categoriasAdmitidas contains r;
									
									
									agregoAlgo = classes.addUna(f1, over, admitido, targets) || agregoAlgo;
								} 
							} else {
								var set2: HashSet[Formula] = new HashSet();
								cl.e2.foreach { 
									entry => { 
										val f2: Formula = new Existential(r, entry.formula);
										set2 += f2;
									}
								}
	
								val admitido: Boolean = categoriasAdmitidas contains r;
								agregoAlgo = classes.addUna(f1, over, admitido, targets) || agregoAlgo;
							}
							
						}
					}
					
				}
			}
				//REVISE: Agregue agregoAlgo=True, por si sale por aca en caso de agrego a la derecha
			} catch { case e: Exception => /*doprint("atrapado por exception: "+ e.toString());*/}
			//REEMPLAZAR ESTO POR CHEQUEO SOLO DE s1
			if (allSingletons == false) {
				allSingletons = classes.isAllSingletons;
			}
			//agregue esto
			isTarget=classes.isTarget(targets);
			if (termino == false) {
			  //agregue aca
				termino = allSingletons || (!agregoAlgo && todasProb1) || isTarget;
				//if (termino)
				//	rolesToProbUse = rolesToProbUseOrig;
			}
		  
			// Actualizo las probabilidades en un 10%
			if (!termino && !agregoAlgo) {
				over = false;
				rolesOrdenados.foreach { r =>
					//rolesToRandUse(r) = Math.abs(new Random().nextFloat());//rand1;
				  	//print(r);
			  		rolesToProbUse(r) = rolesToProbUse(r) + rolesToSaltitosProb(r);
				}
			}
			//else {
				//over = true;
			//}			
		}
		//doprint("Cantidad que dio green ball "+count+" de "+it*2 );
		//file_random_prob.close();
		//if (!allSingletons)
		 // println("no son todos singletons!!!");
		classes.getClasses;
	}


	def computeForJava = {
			val ret = new java.util.HashMap[String, Formula]();
			val classes = compute;

			classes.foreach { entry =>
				entry.e1.extension.scalaIterator.foreach { x =>
				ret.put(x, entry.e1.formula);
				}
			}
			ret
	}
}
