package dlgre;

class BitSetSet[E](domainsize:Int, mapper:E => Int, reverse:Int => E) extends java.util.AbstractSet[E] {
  	private val b = new java.util.BitSet(domainsize);
        private val tmp = new java.util.BitSet(domainsize);
        
        class BitSetSetIterator extends java.util.Iterator[E] {
          private var pos = b.nextSetBit(0);
          
          override def hasNext() = (pos >= 0);
          
          override def next() = {
            val ret = pos;
            pos = b.nextSetBit(pos+1);
            
            reverse(ret)
          }
          
          override def remove() = throw new java.lang.UnsupportedOperationException;
        }
        
        class ScalaBitSetSetIterator extends scala.Iterator[E] {
          private var pos = b.nextSetBit(0);
          
          override def hasNext() = (pos >= 0);
          
          override def next() = {
            val ret = pos;
            pos = b.nextSetBit(pos+1);
            
            reverse(ret)
          }
        }
          
        override def iterator() : java.util.Iterator[E] = {
          new BitSetSetIterator
          /*
          val ret = new java.util.HashSet[E]
          
          Iterator.range(0,domainsize).foreach { x =>
            if( b.get(x) ) {
              ret.add(reverse(x));
            }
          }
          
          ret.iterator()
          */
        }
        
        def scalaIterator : scala.Iterator[E] = new ScalaBitSetSetIterator;
        
        override def size() : Int = {
          b.cardinality()
        }
        
        override def add(x:E) = {
          val index = mapper(x);
          val ret = b.get(index);
          
          b.set(mapper(x));
          
          ret
        }
        
        override def contains(x:Any) = {
          if( x.isInstanceOf[E] ) {
            b.get(mapper(x.asInstanceOf[E]));  
          } else {
            false
          }
        }
          
        def addAll(c:Iterator[E]) = {
          c.foreach { x => add(x) }
        }

        def addAll(c:scala.Iterable[E]) = {
          c.foreach { x => add(x) }
        }
        
        def addAll(other:BitSetSet[E]) = {
          if( domainsize != other.getDomainsize || mapper != other.getMapper || reverse != other.getReverseMapper ) {
            throw new UnsupportedOperationException("incompatible bitsets");
          }
          
          b.or(other.b);
        }

        def getDomainsize : Int = domainsize
        def getMapper = mapper
        def getReverseMapper = reverse
        
        
        def exists(prop: E => Boolean) = {
          (new ScalaBitSetSetIterator).exists(prop)
        }
        
        def forall(prop: E => Boolean) = {
          (new ScalaBitSetSetIterator).forall(prop)
        }
        
        def intersect(other:BitSetSet[E]) = {
          val ret = new BitSetSet[E](domainsize, mapper, reverse)
          
          ret.b.or(b);
          ret.b.and(other.b);
          
          ret;
        }
        
        def intersectWith(other:BitSetSet[E]) = {
          b.and(other.b);
        }
        
        def complement() = {
          b.flip(0, b.size());
        }
        
        // ATTN: not thread-safe
        def isSubsetOf(other:BitSetSet[E]) = {
          tmp.clear();
          tmp.or(other.b);
          tmp.flip(0,domainsize+1);
          tmp.and(b);
          
          tmp.cardinality() == 0
        }
        
        def intersects(other:BitSetSet[E]) = {
          b.intersects(other.b)
        }
        
        def asScalaCollection = {
          new scala.collection.mutable.JavaSetAdaptor[E](this)
        }
        
        override def clear() = b.clear();
            
        override def equals(that: Any): boolean =
          that.isInstanceOf[BitSetSet[E]] && {
            val other = that.asInstanceOf[BitSetSet[E]];
            
            domainsize == other.getDomainsize && mapper == other.getMapper &&
              reverse == other.getReverseMapper && b == other.b
        }
        
        override def hashCode = b.hashCode
}
