package cc.factorie.app.nlp.xcoref

import cc.factorie.infer.SettingsSampler
import cc.factorie.variable.{DiffList, SettingIterator}
import scala.collection.mutable

/**
 * @author John Sullivan
 */
trait CarefulMoveGenerator[Vars <: NodeVariables[Vars], N <: Node[Vars]]  extends MoveGenerator[Vars, N]{
  this :SettingsSampler[(N, N)] =>

  protected def expandedContext(context: (N, N)): Iterable[(N, N)] = Vector(context)

  def moves: IndexedSeq[Move[N]] = Vector.empty[Move[N]]

  override def settings(c:(N, N)) = new SettingIterator with MoveSettingIterator[Vars, N] {
    val (e1, e2) = c

    val moves = new scala.collection.mutable.ArrayBuffer[Move[N]]()

    if(e1.root != e2.root) {
      if(e1.isMention && e1.isRoot && e2.isMention && e2.isRoot) {
        moves += new MergeUp[Vars, N](e1, e2)({d => newInstance(d)})
      } else {
        if(e1.mentionCountVar.value > e2.mentionCountVar.value) {
          moves += new MergeLeft[Vars, N](e1, e2)
        } else {
          moves += new MergeLeft[Vars, N](e2, e1)
        }
      }
    } else {
      if(e1.mentionCountVar.value > e2.mentionCountVar.value) {
        moves += new SplitRight[Vars, N](e2, e1)
      } else {
        moves += new SplitRight[Vars, N](e1, e2)
      }
    }

    moves += new NoMove[Vars, N]
  }
}
