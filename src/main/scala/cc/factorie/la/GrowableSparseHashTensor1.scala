package cc.factorie.la

import scala.collection.mutable
import cc.factorie.util.{DoubleSeq, SeqIntSeq, IntSeq, SparseDoubleSeq}

/**
 * @author John Sullivan
 */
class GrowableSparseHashTensor1(val sizeProxy:Iterable[Any]) extends Tensor1 with SparseDoubleSeq{

  private val h = new mutable.LinkedHashMap[Int, Double]/*{
    override def default(i:Int) = 0.0
    override protected def initialSize: Int = sizeProxy.size
  }
  */
  private var l1Norm = 0.0
  private var l2Norm = 0.0

  def activeDomain: IntSeq = new SeqIntSeq(h.keys.toIndexedSeq) // TODO This is currently really inefficient

  def activeDomainSize: Int = h.size

  def isDense: Boolean = false


  override def copy: Tensor1 = {
    val t = new GrowableSparseHashTensor1(h.toIterable)
    h.foreach{ case(index, value) =>
      t.+=(index,value)
    }
    t
  }

  def dot(ds: DoubleSeq): Double = ds match {
    case t:SparseBinaryTensor1 => t dot this
    case v:TensorTimesScalar => v dot this
    case v:SingletonBinaryTensor1 => v dot this
    case v:SingletonTensor1 => v dot this
    case that:GrowableSparseHashTensor1 => if (that.size > this.size) h.foldLeft(0.0){case (run, (idex, thisVal)) => run + that(idex) * thisVal}
    else that.h.foldLeft(0.0){case (run, (idex, thisVal)) => run + h(idex) * thisVal}
    case dv:DoubleSeq => h.foldLeft(0.0){case (run, (idex, thisVal)) => run + dv(idex) * thisVal}
  }

  def deductedDot(ds:DoubleSeq, deduction:DoubleSeq = this):Double = ds match {
    case that:GrowableSparseHashTensor1 => if (that.size > this.size) h.foldLeft(0.0){case (run, (index, thisVal)) => run + (that(index) - deduction(index)) * thisVal}
    else that.h.foldLeft(0.0){case (run, (index, thisVal)) => run + (h(index) - deduction(index)) * thisVal}
    case dv:DoubleSeq => h.foldLeft(0.0){case (run, (index, thisVal)) => run + (dv(index) - deduction(index)) * thisVal}
  }

  override def twoNormSquared:Double = l2Norm
  override def oneNorm:Double = l1Norm

  def deductedCosineSimilarity(ds:DoubleSeq, deduction:DoubleSeq = this):Double = this.deductedDot(ds, deduction) / (this.twoNorm * ds.twoNorm)

  protected def getVal(index:Int):Double = h.getOrElse(index, 0.0)

  override def +=(index:Int, incr:Double): Unit = {
    //assert(index < length, "index %d should be less than length %d".format(index, length))

    if(incr!=0.0){
      l1Norm += incr
      l2Norm += incr*incr + 2.0*getVal(index)*incr
      h(index) = getVal(index) + incr
      if(withinEpsilon(0.0, h(index))) h.remove(index)
    }
  }

  override def -=(index:Int, incr:Double): Unit = {
    if(incr!=0.0){
      l1Norm -= incr
      l2Norm -= (incr*incr + 2.0*getVal(index)*incr)
      if(withinEpsilon(incr, getVal(index))) h.remove(index)
      else h(index) = getVal(index) - incr
    }
  }

  @inline private final def withinEpsilon(v1:Double, v2:Double, epsilon:Double=0.000001):Boolean = if(v1==v2)true else (v1 - v2).abs <= epsilon

  def zero(): Unit = h.clear()

  override def update(index:Int, value:Double) = {
    assert(index < length, "index %d should be less than length %d".format(index, length))
    if(value == 0.0) h.remove(index)
    else h(index) = value
  }

  def dim1: Int = sizeProxy.size

  def apply(i: Int): Double = h(i)

  def foreachActiveElement(f: (Int, Double) => Unit): Unit = h.filter{case (_, value) => value != 0.0} foreach f.tupled
}