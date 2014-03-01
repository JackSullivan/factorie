package cc.factorie.app.nlp.xcoref

import scala.collection.mutable
import cc.factorie._
import cc.factorie.infer.{SettingsSampler, Proposal}

/**
 * @author John Sullivan
 */
trait LegacyPairGenerator[N <: Node[_]] extends ContextGenerator[N, (N ,N)] {
  this:SettingsSampler[(N,N)] =>

  proposalHooks += {p:Proposal[(N, N)] =>
    val (e1, e2) = p.context
    if(e1.parent.isDefined){
      addEntity(e1.parent.get.asInstanceOf[N])
    }
    if(e2.parent.isDefined){
      addEntity(e2.parent.get.asInstanceOf[N])
    }
  }

  protected val _allEntities = mutable.HashSet[N]()

  _allEntities ++= mentions

  def addEntity(e:N) {
    if(!_allEntities.contains(e)) {
      _allEntities += e
    }
  }
  def allEntities:Iterable[N] = _allEntities

  def sampleEntity:N = {
    var e = _allEntities.sampleUniformly
    while(!e.existsVar.booleanValue) {
      _allEntities.remove(e)
      e = _allEntities.sampleUniformly
    }
    e
  }

  def nextContext: (N, N) = {
    val e1 = sampleEntity
    var e2 = sampleEntity
    while(e1 == e2) {
      e2 = sampleEntity
    }
    e1 -> e2
  }
}
