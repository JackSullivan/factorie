package cc.factorie.variable

import cc.factorie.la.Tensor1

/**
 * @author John Sullivan
 */
trait BagVariable[B <: BagVariable[B]] extends MutableTensorVar {
  override type Value = Tensor1

  def newInstance(tensor:Tensor1)(implicit d:DiffList):B

  def ++=(other:B)(implicit d:DiffList){
    this.set(this.value + other.value)(d)
  }
  def --=(other:B)(implicit d:DiffList){
    this.set(this.value - other.value)(d)
  }

  def ++(other:B)(implicit d:DiffList):B = newInstance(this.value + other.value)(d)

  def --(other:B)(implicit d:DiffList):B = newInstance(this.value - other.value)(d)
}