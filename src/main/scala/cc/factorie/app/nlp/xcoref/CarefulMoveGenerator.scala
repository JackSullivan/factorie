package cc.factorie.app.nlp.xcoref

import cc.factorie.infer.SettingsSampler
import cc.factorie.variable.{DiffList, SettingIterator}
import scala.collection.mutable

/**
 * @author John Sullivan
 */
trait CarefulMoveGenerator[Vars <: NodeVariables[Vars], N <: Node[Vars]]  extends MoveGenerator[Vars, N]{
  this :SettingsSampler[(N, N)] =>

  protected def expandedContext(context: (N, N)): Iterable[(N, N)] = {
    val (one, two) = context
    one.lineage.map(_.asInstanceOf[N] -> two)
    /*
    val (left, right) = if(one.mentionCountVar.value < two.mentionCountVar.value) { // the left node is the smaller
      one -> two
    } else if(one.mentionCountVar.value > two.mentionCountVar.value) {
      two -> one
    } else if(one.depth > two.depth) { // failing that the left node should be deeper
      one -> two
    } else {
      two -> one
    }
    right.lineage.map(left -> _.asInstanceOf[N])
    */

  }

  def moves: IndexedSeq[Move[N]] = Vector.empty[Move[N]]

  override def settings(context: (N, N)): SettingIterator = new SettingIterator {
    val (e1, e2) = context
    val expanded = expandedContext(context)
    val moves = mutable.ArrayBuffer[DiffList => Unit]()


    def next(d: DiffList): DiffList = ???

    def hasNext: Boolean = ???

    def reset(): Unit = ???
  }
  /*
  override def settings(context: (N, N)): SettingIterator = new SettingIterator {
    val (e1, e2) = context
    val expanded = expandedContext(context)
    val moveList = if(e1.root != e2.root) {
      expanded.map { case (left, right) =>
        if(!right.isMention) {
          Move.mergeLeft[Vars, N](right,left)
        } else { // if right is a mention, then left is also a mention
          Move.mergeUp[Vars, N](right, left, {d:DiffList => newInstance(d)})
        }
      }.toVector
    } else {
      Vector(Move.splitRight[Vars, N](expanded.head._1, expanded.head._2))
    }

    var index = 0
    def next(priorDiff: DiffList): DiffList = {
      val diff = moveList(index).apply(newDiffList)
      index += 1
      //println("Composed difflist of length %s" format diff.size)
      diff
    }

    def hasNext: Boolean = index <= moves.size

    def reset() {index = 0}
  }
  */
}
