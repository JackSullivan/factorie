package cc.factorie.app.nlp.xcoref

import cc.factorie.variable.{SettingIterator, DiffList}
import cc.factorie.infer.SettingsSampler
import scala.collection.mutable

/**
 * User: escher, John Sullivan, akobren
 * Date: 10/28/13
 *
 */
trait MoveGenerator[Vars <: NodeVariables[Vars], N<:Node[Vars]]  {

  this: SettingsSampler[(N, N)] =>

  def newInstance(implicit d:DiffList):N


  def settings(context: (N, N)): SettingIterator = new SettingIterator {
    private val proposals:Iterable[DiffList => DiffList] = expandedContext(context).flatMap{ case(n1, n2) =>
      moves.flatMap{ move =>
        if(move.isValid(n1, n2)) List(move(n1,n2)(_)) else Nil ++ (if(!move.isSymmetric(n1,n2)&&move.isValid(n2,n1)) List(move(n2, n1)(_)) else Nil)
      }
    }
    private val moveObjs = expandedContext(context).flatMap{ case(n1, n2) =>
      moves.flatMap{ move =>
        if(move.isValid(n1, n2)) List(move) else Nil ++ (if(!move.isSymmetric(n1,n2)&&move.isValid(n2,n1)) List(move) else Nil)
      }
    }
    //println("With a context of size: %d, we generated these moves:".format(expandedContext(context).size), moveObjs.groupBy(_.name).mapValues(_.size))

    if(proposals.size == 1) {
      println("WARNING: Only 1 move was generated!!")
      val (n1, n2) = context
      println("n1 is a mention: %s, n1 is a root: %s".format(n1.isMention, n1.isRoot))
      println("n2 is a mention: %s, n2 is a root: %s".format(n2.isMention, n2.isRoot))
      println("n1 and n2 share a parent: %s".format(n1.root == n2.root))
    }

    private var proposalsIter = proposals.toIterator

    def next(d: DiffList): DiffList = proposalsIter.next()(d)

    def hasNext: Boolean = proposalsIter.hasNext

    def reset() {
      proposalsIter = proposals.toIterator
    }
  }

  protected def expandedContext(context:(N, N)):Iterable[(N,N)]
  def moves: IndexedSeq[Move[N]]
}

trait Move[N <: Node[_]] {
  def name: String

  def isSymmetric(node1:N, node2:N): Boolean // is the move symmetric for this pair of nodes?

  def isValid(node1: N, node2:N): Boolean
  def operation(node1: N, node2:N)(d:DiffList): DiffList
  final def apply(node1:N, node2:N)(d:DiffList):DiffList = Option(d) match {
    case Some(diff) => operation(node1, node2)(diff)
    case None => operation(node1, node2)(new DiffList)
  }
}

object Move {
  def splitRight[Vars <: NodeVariables[Vars], N <: Node[Vars]](right:N, left:N):(DiffList => DiffList) = {
    val m = new SplitRight[Vars, N]
    assert(m.isValid(right, left))
    m.operation(right, left)
  }
  def mergeLeft[Vars <: NodeVariables[Vars], N <: Node[Vars]](right:N, left:N):(DiffList => DiffList) = {
    val m = new MergeLeft[Vars, N]
    assert(m.isValid(left, right))
    m.operation(left, right)
  }
  def mergeUp[Vars <: NodeVariables[Vars], N <: Node[Vars]](right:N, left:N, newInstance:(DiffList => N)):(DiffList => DiffList) = {
    val m = new MergeUp[Vars, N](newInstance)
    assert(m.isValid(right, left))
    m.operation(right, left)
  }
}

trait DefaultMoveGenerator[Vars <: NodeVariables[Vars], N <: Node[Vars]] extends MoveGenerator[Vars, N] {
  this: SettingsSampler[(N,N)] =>
  val moves = IndexedSeq(new NoMove[Vars, N], new MergeLeft[Vars, N], new SplitRight[Vars, N], new MergeUp[Vars, N]({d:DiffList => this.newInstance(d)}))

  @inline
  protected def expandedContext(context: (N, N)): Iterable[(N, N)] = List(context)
}

trait RootCheckingMoveGenerator[Vars <: NodeVariables[Vars], N <: Node[Vars]] extends MoveGenerator[Vars, N] {
  this: SettingsSampler[(N, N)] =>
  override protected def expandedContext(context: (N, N)): Iterable[(N, N)] = {
    val (n1, n2) = context
    val r1 = n1.root; val r2 = n2.root
    val l = new mutable.ArrayBuffer[(N,N)](3)
    if(r1 != n1) l += r1.asInstanceOf[N] -> n2
    if(r2 != n2) l += r2.asInstanceOf[N] -> n1
    l += context
    l
  }
}

trait ParentCheckingMovingGenerator[Vars <: NodeVariables[Vars], N <: Node[Vars]] extends MoveGenerator[Vars, N] {
  this: SettingsSampler[(N, N)] =>

  override protected def expandedContext(context: (N, N)): Iterable[(N, N)] = {
    val (n1, n2) = context
    List(n1 -> n2, n1.root.asInstanceOf[N] -> n2.root.asInstanceOf[N]) ++ n1.lineage.map(_.asInstanceOf[N] -> n2.asInstanceOf[N]) //++ n2.lineage.map(_.asInstanceOf[N] -> n1.asInstanceOf[N])
  }
}

/*
trait ParentCheckingMoveGenerator[Vars <: NodeVariables[Vars], N <: Node[Vars]] extends MoveGenerator[Vars, N] {
  this:SettingsSampler[(N,N)] =>
  override protected def expandedContext(context: (N, N)): Iterable[(N, N)] = {
    val (n1, n2) = context
    n1.lineage.map(_.asInstanceOf[N] -> n2) ++ n2.lineage.map(_.asInstanceOf[N] -> n1)
  }
}
*/
/*
trait ParentCheckingMoveGenerator[Vars <: NodeVariables[Vars], N <: Node[Vars]] extends DefaultMoveGenerator[Vars, N] {

  override def generate(n1: N, n2: N): Iterator[DiffList] = new Iterator[DiffList] {
    val movePairs = n1.lineage.map(_ -> n2) ++ n2.lineage.map(_ -> n1)
    val proposals = movePairs.flatMap { case(nV1, nV2) =>
      val n1 = nV1.asInstanceOf[N]
      val n2 = nV2.asInstanceOf[N]
      moves.flatMap(_.apply(n1,n2)) ++ moves.filterNot(_.isSymmetric(n1, n2)).flatMap(_.apply(n2,n1))
    }.toIterator


    def hasNext: Boolean = proposals.hasNext

    def next(): DiffList = proposals.next()
  }
}
*/
class NoMove[Vars <: NodeVariables[Vars], N <: Node[Vars]] extends Move[N] {
  def name = "No Move"

  def isSymmetric(node1: N, node2: N): Boolean = true

  def isValid(node1: N, node2: N): Boolean = true
  def operation(node1: N, node2: N)(d:DiffList) = {
    d
  }
}

class MergeLeft[Vars <: NodeVariables[Vars], N <: Node[Vars]] extends Move[N]{
  def name = "Merge Left"
  def isValid(right: N, left: N) = right.root != left.root && !left.isMention && left.mentionCountVar.value >= right.mentionCountVar.value
  def isSymmetric(node1: N, node2: N): Boolean = false

  def operation(right: N, left: N)(d:DiffList) = {
    right.alterParent(Option(left))(d)
    d
  }
}

class SplitRight[Vars <: NodeVariables[Vars], N <: Node[Vars]] extends Move[N] {
  def name = "Split Right"
  def isValid(right: N, left: N): Boolean = left.root == right.root && right.mentionCountVar.value >= left.mentionCountVar.value
  def isSymmetric(node1: N, node2: N): Boolean = false

  def operation(right:N, left: N)(d:DiffList) = {
    right.alterParent(None)(d)
    d
  }
}

class MergeUp[Vars <: NodeVariables[Vars], N <: Node[Vars]](newInstance:(DiffList => N)) extends Move[N] {
  def name = "Merge up"
  def isValid(right: N, left: N): Boolean = left.root != right.root && (left.isRoot && right.isRoot) && (left.isMention && right.isMention)
  def isSymmetric(node1: N, node2: N): Boolean = true

  def operation(right: N, left: N)(d:DiffList) = {
    val newParent = newInstance(d)
    right.alterParent(Some(newParent))(d)
    left.alterParent(Some(newParent))(d)
    d
  }
}
