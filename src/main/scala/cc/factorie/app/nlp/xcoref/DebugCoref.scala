package cc.factorie.app.nlp.xcoref

import cc.factorie.infer.Proposal

/**
 * @author John Sullivan
 */
trait DebugCoref[Vars <: NodeVariables[Vars], N <: Node[Vars]]{
  this: CorefSampler[Vars, N] with ContextGenerator[N, (N, N)] with MoveGenerator[Vars, N]=>

  var printEvery:Int = 1000

  var acceptedProps = 0.0
  var totalProps = 0
  lazy val begin = System.currentTimeMillis()
  var startTime = 0L
  var stopTime = 0L

  proposalHooks += {p:Proposal[(N, N)] =>
    totalProps +=1
    if(p.diff.size != 0) acceptedProps += 1
    if(totalProps % printEvery == 0) {
      stopTime = System.currentTimeMillis()
      val elapsedSecs = (stopTime - startTime) / 1000.0
      val elapsedFromBegin = (stopTime - begin) / 1000.0
      val percentAccepted = (acceptedProps / totalProps)*100
      val propsPerSec = printEvery.toDouble / elapsedSecs
      val totalPropsPerSec = totalProps.toDouble / elapsedFromBegin
      val depths = mentions.map(_.depth)
      val maxDepth = depths.max
      val minDepth = depths.min
      val aveDepth = depths.sum.toDouble / depths.size
      val roots = mentions.map(_.root).toSet
      val rootChildrens = roots.map(_.children.size)
      val rootMentions = roots.map(_.mentionCountVar.value)
      val maxChildren = rootChildrens.max
      val minChildren = rootChildrens.min
      val aveChildren = rootChildrens.sum.toDouble / rootChildrens.size
      val maxMentions = rootMentions.max
      val minMentions = rootMentions.min
      val aveMentions = rootMentions.sum.toDouble / rootMentions.size
      println(f"After $totalProps%d proposals $percentAccepted%.2f%% accepted in $elapsedFromBegin%.3f secs ($totalPropsPerSec%.2f proposals/sec). This round of $printEvery%d took $elapsedSecs%.3f secs ($propsPerSec%.2f proposals/sec)")
      println(f"\t max depth: $maxDepth min depth: $minDepth ave depth: $aveDepth%.2f")
      println(f"\t max children: $maxChildren min children: $minChildren ave children: $aveChildren%.2f")
      println(f"\t max mentions: $maxMentions min mentions: $minMentions ave mentions: $aveMentions%.2f")
      startTime = stopTime
    }
  }

  /*proposalHooks += {p: Proposal[(N,N)] =>
    println(NodeTreeStatistics.printStats(this.mentions.map(_.root).toSet.toList))
  }

  proposalHooks += {p: Proposal[(N,N)] =>
    NodeTreeStatistics.printBiggestTree(this.mentions.map(_.root).toSet.toList)
  } */

  beforeInferHooks += { _ =>
    startTime = begin
  }


 /*
  override def proposalHook(proposal:Proposal[(N, N)]){
    var t1 = System.currentTimeMillis()
    proposalHooks.apply(proposal)
    var t2 = System.currentTimeMillis()
    if(totalProps % printEvery == 0) println("Proposal hooks took %.5f seconds".format((t2 - t1)/1000.0))
  }

  override def contexts: Iterable[(N, N)] = new Iterator[(N, N)] {

    var index = 0

    def hasNext: Boolean = index < iterations

    def next(): (N, N) = if(hasNext) {
      index += 1
      var t1 = System.currentTimeMillis()
      val n = nextContext
      var t2 = System.currentTimeMillis()
      if(totalProps % printEvery == 0) println("nextContext took %.5f seconds".format((t2 - t1)/1000.0))
      n
    } else {
      throw new NoSuchElementException("Max iterations exceeded %d" format iterations)
    }
  }.toStream

  override def infer {
    var t1 = System.currentTimeMillis()
    beforeInferHook
    var t2 = System.currentTimeMillis()
    if(totalProps % printEvery == 0) println("BeforeInferHook took %.5f seconds".format((t2 - t1)/1000.0))
    t1 = t2
    processAll(contexts)
  }

  override def settings(context: (N, N)): SettingIterator = new SettingIterator {
    var t1 = System.currentTimeMillis()
    private val proposals:Iterable[DiffList => DiffList] = expandedContext(context).flatMap{ case(n1, n2) =>
      moves.flatMap{ move =>
        if(move.isValid(n1, n2)) List(move(n1,n2)(_)) else Nil ++ (if(!move.isSymmetric(n1,n2)&&move.isValid(n2,n1)) List(move(n2, n1)(_)) else Nil)
      }
    }
    var t2 = System.currentTimeMillis()
    if(totalProps % printEvery == 0) println("Generated %d proposals in %.5f secs".format(proposals.size, (t2 - t1)/1000.0))

    private var proposalsIter = proposals.toIterator

    def next(d: DiffList): DiffList = proposalsIter.next()(d)

    def hasNext: Boolean = proposalsIter.hasNext

    def reset() {
      proposalsIter = proposals.toIterator
    }
  }

  trait DebugMove extends Move[N] {
    abstract override def operation(node1: N, node2: N)(d: DiffList): DiffList = {
      var t1 = System.currentTimeMillis()
      val o = super.operation(node1, node2)(d)
      var t2 = System.currentTimeMillis()
      if(totalProps % printEvery == 0) println("Performed %s operation in %.5f secs".format(name, (t2 - t1)/1000.0))
      o
    }
  }

  override val moves = IndexedSeq(new NoMove[Vars, N] with DebugMove,
    new MergeLeft[Vars, N] with DebugMove,
    new SplitRight[Vars, N] with DebugMove,
    new MergeUp[Vars, N]({d:DiffList => this.newInstance(d)}) with DebugMove)

    */
}
