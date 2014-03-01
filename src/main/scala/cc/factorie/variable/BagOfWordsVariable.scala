package cc.factorie.variable

import cc.factorie.Cubbie
import cc.factorie.app.nlp.xcoref.NodeCubbie
import cc.factorie.la.{ArraySparseIndexedTensor, Tensor1, GrowableSparseTensor1, GrowableSparseHashTensor1}

/**
 * @author John Sullivan
 */
object GlobalVocabulary extends CategoricalVectorDomain[String]
class BagOfWordsVariable(tensor:Tensor1, val domain:CategoricalVectorDomain[String] = GlobalVocabulary)(implicit d:DiffList)
  extends CategoricalVectorVariable[String] with BagVariable[BagOfWordsVariable] {

  _bow =>
  set(tensor)(d)
  def this(words:Iterable[String])(implicit d:DiffList) = {
    this(new GrowableSparseHashTensor1(words))(d)
    this ++= words
  }

  def this(words: Iterable[String], domain: CategoricalVectorDomain[String])(implicit d: DiffList) = {
    this(new GrowableSparseTensor1(domain.dimensionDomain))(d)
    this ++= words
  }

  override def toString:String = "%s(%s)".format(printName, {
    val wordScores = new StringBuilder()
    value.foreachActiveElement((i, v) =>
      wordScores.append("%s=%.2f,".format(domain.dimensionDomain.category(i), v))
    )
    wordScores.toString()
  })

  def asHashMap = (value.activeElements map {
    case (i, v) => domain.dimensionDomain.category(i) -> v
  }).toMap

  def newInstance(t: Tensor1)(implicit diff: DiffList): BagOfWordsVariable = new BagOfWordsVariable(t, domain)(d)

  def deductedDot(other:BagOfWordsVariable, deduct:BagOfWordsVariable=this):Double = {
    var result = 0.0
    this.value match {case t:ArraySparseIndexedTensor => t._makeReadable(); case _ => Unit}
    other.value match {case t:ArraySparseIndexedTensor => t._makeReadable(); case _ => Unit}
    deduct.value match {case t:ArraySparseIndexedTensor => t._makeReadable(); case _ => Unit}

    this.value.foreachActiveElement{ case(index, v1) =>
      result += v1 * (other.value(index) - deduct.value(index))
    }
    result
  }

  def deductedCosineSimilarity(other: BagOfWordsVariable, deduct: BagOfWordsVariable = this): Double = {
    def simpleDot(v1:Tensor1, v2:Tensor1):Double = { // Temporary fix since factorie's dot implementation isn't behaving
      var result = 0.0
      v1.foreachActiveElement{ case (index, value) =>
        result += value * v2(index)
      }
      result
    }

    val numerator = this.deductedDot(other, deduct)
    if (numerator != 0.0) {
      val otherL2Norm = Math.sqrt(deduct.value.twoNorm*deduct.value.twoNorm + other.value.twoNorm*other.value.twoNorm - 2 * simpleDot(deduct.value, other.value))
      val denominator:Double = this.value.twoNorm * otherL2Norm
      if(denominator==0.0 || denominator != denominator) 0.0 else numerator/denominator
    } else 0.0
  }
}

object BagOfWordsVariable {
  def main(args:Array[String]) {
    val words = Seq("this", "that", "the", "other")

    val bow = new BagOfWordsVariable(words)(null)

    println(bow.asHashMap)
  }
}

class BOWCubbie extends Cubbie{
  val nodeId = RefSlot[NodeCubbie[_,_]]("nid",()=>null.asInstanceOf[NodeCubbie[_,_]])  //todo understand how this null works
  val word   = StringSlot("w")
  val count  = DoubleSlot("c")
  def fetch = this.word -> this.count
  def store(id:String, w:String, c:Double) = {
    nodeId := id
    word   := w
    count  := c
    this
  }
}