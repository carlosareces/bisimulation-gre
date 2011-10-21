package dlgre.realize;

import grapht._;
import scala.collection.mutable._;
import dlgre.formula._;
import util.StringUtils._;

object Realizer {
  val propsToNouns = new HashMap[String,String];
  val propsToAdjectives = new HashMap[String,String];
  val nounProps = new HashSet[String];
  
  val rolesToPreps = new HashMap[String,String];
  
  
  // the lexicon
  propsToNouns += "drawer" -> "drawer";
  nounProps += "drawer";
  
  propsToAdjectives += "green" -> "green";
  propsToAdjectives += "blue" -> "blue";
  propsToAdjectives += "orange" -> "orange";
  propsToAdjectives += "pink" -> "pink";
  
  propsToAdjectives += "top_row" -> "in the top row";
  propsToAdjectives += "bottom_row" -> "in the bottom row";
  propsToAdjectives += "leftmost_col" -> "in the first column";
  propsToAdjectives += "rightmost_col" -> "in the last column";
  
  rolesToPreps += "above" -> "above";
  rolesToPreps += "below" -> "below";
  rolesToPreps += "next" -> "next to";
  rolesToPreps += "above" -> "above";
  rolesToPreps += "left" -> "to the left of";
  rolesToPreps += "right" -> "to the right of";
  
  
	def realize(concept:Formula, as:String, defaultnoun:String) :String = {
          if( as == "np" ) {
            "the " + realize(concept, "noun", defaultnoun)
          } else {
            concept match {
              case Literal(p,pol) => if( as == "noun" ) propsToNouns.get(p).get else propsToAdjectives.get(p).get;
              case Conjunction(subs) => realizeConjunction(subs, defaultnoun);
              case Existential(role, sub) => rolesToPreps.get(role).get + " " + realize(sub,"noun",defaultnoun);
              case _ => "(unk: " + concept + ")";
            }
          }
        }
        
        private def realizeConjunction(subs:List[Formula], defaultnoun:String) :String = {
          val nountest = {x:Formula =>
            x match {
              case Literal(p,pol) => nounProps.contains(p);
              case _ => false;
            }
          };
          
          val nounables = subs.filter(nountest);
          val theNoun = nounables match {
            case h::_ => realize(h,"noun",defaultnoun);
            case Nil => defaultnoun;
          }
          
          val postmodifiers = subs.filter { f => f.isInstanceOf[Existential] };
          val premodifiers = subs.filter { f => (f != theNoun) && !postmodifiers.contains(f) };
          
          val buf = new StringBuffer("the");
          
          premodifiers.foreach { f => buf.append(" " + realize(f, "adjective", defaultnoun)) };
          buf.append(" " + theNoun);
          postmodifiers.foreach { f => buf.append(" " + realize(f, "adjective", defaultnoun)) };
          
          
          buf.toString();
        }
}
