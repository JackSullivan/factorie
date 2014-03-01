package cc.factorie.util

import cc.factorie.la.Tensor

/**
 * @author John Sullivan
 */
trait TFIDF {

  def frequency(termIndex:Int, document:Tensor):Double

  def termFrequency(termIndex:Int, document:Tensor):Double = 0.5 + (0.5 * frequency(termIndex, document) / document.max)

  def inverseDocumentFrequency(termIndex:Int, corpus:Iterable[Tensor]):Double = math.log(corpus.size.toDouble / (1 + corpus.count(_.apply(termIndex) != 0.0)))

  def apply(corpus:Iterable[Tensor]):Iterable[Tensor] = {
    val activeIndices = corpus.foldLeft(Set.empty[Int]){ _ ++ _.activeDomain.toSeq}
    val idfCounts = activeIndices.map(index => index -> inverseDocumentFrequency(index, corpus)).toMap
    corpus.map{ document =>
      val dtfidf = document.copy
      dtfidf.foreachActiveElement{ case(index, _) =>
        dtfidf.update(index, termFrequency(index, document) * idfCounts(index))
      }
      dtfidf
    }
  }
}

object RawTFIDF extends TFIDF {
  def frequency(termIndex: Int, document: Tensor): Double = document(termIndex)
}

object BooleanTFIDF extends TFIDF {
  def frequency(termIndex: Int, document: Tensor): Double = if(document(termIndex) != 0.0) 1.0 else 0.0
}