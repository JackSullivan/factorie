package cc.factorie.app.nlp.xcoref

import cc.factorie.variable.{SettingIterator, DiffList}

/**
 * @author John Sullivan
 */
trait MoveSettingIterator[Vars <: NodeVariables[Vars], N <: Node[Vars]] extends SettingIterator{
  def moves:IndexedSeq[Move[N]]

  var i = 0

  def hasNext = i < moves.size
  def next(diff:DiffList) = {val d = newDiffList; moves(i).perform(d); i += 1; d}
  def reset = i = 0
}