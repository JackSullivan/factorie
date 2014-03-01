package cc.factorie.app.nlp.xcoref

import cc.factorie.variable.{TensorVar, RealValue, ArrowVariable}
import cc.factorie.model._
import cc.factorie._
import scala.reflect.ClassTag
import cc.factorie.la.{Tensor, Tensor1}
import scala.Some
import cc.factorie.Parameters
import cc.factorie.app.nlp.hcoref.{BagOfWordsVariable => OldBagOfWordsVariable, DebugableTemplate}

/**
 * @author John Sullivan
 */
abstract class ChildParentTemplate[Vars <: NodeVariables[Vars]](initWeights:Tensor1)(implicit v1:ClassTag[Vars], params:Parameters)
  extends Template3[ArrowVariable[Node[Vars], Node[Vars]], Vars, Vars]
  with DotFamily3[ArrowVariable[Node[Vars], Node[Vars]], Vars, Vars]{
  override def unroll1(v: ArrowVariable[Node[Vars], Node[Vars]]) = Option(v.dst) match { // If the parent-child relationship exists, we generate factors for it
    case Some(dest) => Factor(v, v.src.variables, dest.variables)
    case None => Nil
  }
  def unroll2(v: Vars) = Nil
  def unroll3(v: Vars) = Nil

  val _weights = params.Weights(initWeights)

  def weights: Weights = _weights
}

class BagOfWordsEntropy[Vars <:NodeVariables[Vars], T <: TensorVar](initialWeight:Double, getBag:(Vars => T))(implicit ct:ClassTag[Vars], params:Parameters)
  extends Template2[Node[Vars]#Exists, Vars]
  with DotFamily2[Node[Vars]#Exists, Vars]
  with DebugableTemplate {


  def name: String = "BagOfWordsEntropy"

  val _weights = params.Weights(Tensor1(initialWeight))
  def weights: Weights = _weights

  def unroll1(v: Node[Vars]#Exists) = Factor(v, v.node.variables)

  def unroll2(v: Vars) = Factor(v.node.existsVar, v)

  override def statistics(exists: Node[Vars]#Exists#Value, vars: Vars#Value) = {
    val bag = getBag(vars).value
    var entropy = 0.0
    var n = 0.0
    if(exists.booleanValue /*&& isEntity.booleanValue*/){
      val l1Norm = bag.oneNorm
      bag.foreachActiveElement{ case(k,v) =>
        entropy -= (v/l1Norm)*math.log(v/l1Norm)
        n+=1.0
      }
    }
    if(n>1)entropy /= scala.math.log(n) //normalized entropy in [0,1]
    if(entropy.isNaN) {
      println("Warning entropy is NaN!")
      println("n=:" + n)
      println("active size: %d\tone norm: %.4f\t".format(bag.activeDomainSize, bag.oneNorm))
      bag.foreachActiveElement{ case (index, value) =>
        println("index:%d\tvalue:%.4f\t(value/oneNorm):%.4f\tlog(value/oneNorm:%.4f".format(index, value, value/bag.oneNorm, math.log(value/bag.oneNorm)))
      }
    }
    entropy = -entropy
    if(_debug)println("  "+debug(entropy))
    new RealValue(entropy)
  }
}

class OldBagOfWordsEntropy[Vars <:NodeVariables[Vars]](initialWeight:Double, getBag:(Vars => OldBagOfWordsVariable))(implicit ct:ClassTag[Vars], params:Parameters)
  extends Template2[Node[Vars]#Exists, Vars]
  with DotFamily2[Node[Vars]#Exists, Vars]
  with DebugableTemplate {


  def name: String = "BagOfWordsEntropy"

  val _weights = params.Weights(Tensor1(initialWeight))
  def weights: Weights = _weights

  def unroll1(v: Node[Vars]#Exists) = Factor(v, v.node.variables)

  def unroll2(v: Vars) = Factor(v.node.existsVar, v)

  override def statistics(exists: Node[Vars]#Exists#Value, vars: Vars#Value) = {
    val bag = getBag(vars).value
    var entropy = 0.0
    var n = 0.0
    if(exists.booleanValue /*&& isEntity.booleanValue*/){
      val l1Norm = bag.l1Norm
      bag.asHashMap.foreach{ case(k,v) =>
        entropy -= (v/l1Norm)*math.log(v/l1Norm)
        n+=1.0
      }
    }
    if(n>1)entropy /= scala.math.log(n) //normalized entropy in [0,1]
    if(entropy.isNaN) {
      println("Warning entropy is NaN!")
      println("n=:" + n)
      println("active size: %d\tone norm: %.4f\t".format(bag.size, bag.l1Norm))
      bag.asHashMap.foreach{ case (index, value) =>
        println("index:%d\tvalue:%.4f\t(value/oneNorm):%.4f\tlog(value/oneNorm:%.4f".format(index, value, value/bag.l1Norm, math.log(value/bag.l1Norm)))
      }
    }
    entropy = -entropy
    if(_debug)println("  "+debug(entropy))
    new RealValue(entropy)
  }
}

abstract class BagOfWordsTemplate[Vars <: NodeVariables[Vars]](initialWeights: Tensor1)(implicit v1: ClassTag[Vars], params: Parameters)
  extends Template1[Vars]
  with DotFamily1[Vars] {
  override def unroll1(v: Vars) = Option(v) match {
    case Some(v) => Factor(v)
    case None => Nil
  }

  val _weights: Weights = params.Weights(initialWeights)

  def weights: Weights = _weights
}

abstract class StructuralTemplate[A <: Var](initialWeights: Tensor1)(implicit v1: ClassTag[A], params: Parameters)
  extends Template1[A]
  with DotFamily1[A] {

  val _weights: Weights = params.Weights(initialWeights)

  def weights: Weights = _weights
}
/* This code is an example of how to use the new ChildParentTemplate with NodeVariables

case class MyNodeVariables(names:BagOfWordsVariable, context:BagOfWordsVariable) extends NodeVariables[MyNodeVariables] {
  def combine(other: MyNodeVariables): MyNodeVariables = {
    this.names ++= other.names.activeCategories
    this.context ++= other.context.activeCategories
    this
  }

  def remove(other: MyNodeVariables): MyNodeVariables = this
}


abstract class GenericModel[Vars <: NodeVariables[Vars]] extends TemplateModel {

}

class ConcreteModel extends GenericModel[MyNodeVariables] {

  this += new ChildParentNameTemplate(3)
}


class ChildParentNameTemplate(val initWeight:Double)(implicit params:Parameters) extends ChildParentTemplate1[MyNodeVariables] {

  def statistics(v1: ArrowVariable[Node[MyNodeVariables], Option[Node[MyNodeVariables]]]#Value, v2: MyNodeVariables, v3: MyNodeVariables):RealValue = new RealValue(v2.names.value cosineSimilarity v3.names.value)

  val _weights:Weights1 = params.Weights(Tensor1(initWeight))

  def weights: Weights = _weights
}

*/
