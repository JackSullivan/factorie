package cc.factorie.app.nlp.xcoref

import scala.util.Random
import cc.factorie._

/**
 * @author John Sullivan
 */
trait ContextGenerator[N <: Node[_], Context] {
  def mentions:Iterable[N]

  def iterations:Int
  implicit def random:Random
  def nextContext:Context

  protected def randomNode:N = {
    val n1 = mentions.sampleUniformly(random)
    //println("Finding Random Node. Depth of N1 is %d" format n1.depth)
    val sampleDepth = random.nextInt(n1.depth)
    val sampleNode = n1.ancestor(sampleDepth).asInstanceOf[N]
    //if(!(sampleNode eq n1)) {
    //  println("Sampled node: %s, depth of %d, is mention: %s".format(sampleNode, sampleDepth, sampleNode.isMention))
    //}
    sampleNode
  }

  protected def randomMention:N = {
    mentions.sampleUniformly(random)
  }

    def contexts:Iterable[Context] = new Iterator[Context] {

    var index = 0

    def hasNext: Boolean = index < iterations

    def next(): Context = if(hasNext) {
      index += 1
      nextContext
    } else {
      throw new NoSuchElementException("Max iterations exceeded %d" format iterations)
    }
  }.toStream
}

trait RandomPairGenerator[N <: Node[_]] extends ContextGenerator[N, (N, N)] {
  def nextContext:(N, N) = {
    if(mentions.size == 1) {throw new Error("Cannot sample pairs from a single node")}
    val n1 = randomNode
    var n2 = randomNode
    while(n1 == n2) {
      n2 = randomNode
    }
    n1 -> n2
  }
}

// Currently unimplemented because of changes to canopization
/*trait CanopyPairGenerator[N <: Node[_] with Canopization[N, _]] extends ContextGenerator[N, (N, N)] {
  def nextContext:(N, N) = {
    if(mentions.size == 1) {throw new Error("Cannot sample pairs from a single node")}
    val n1 = randomNode
    var n2 = n1.canopies.sampleUniformly.sampleUniformly
    n2 = n2.ancestor(random.nextInt(n2.depth) - 1).asInstanceOf[N]
    while(n1 == n2) {
      n2 = n1.canopies.sampleUniformly.sampleUniformly
      n2 = n2.ancestor(random.nextInt(n2.depth) - 1).asInstanceOf[N]
    }
    n1 -> n2
  }
}
  */

trait NodeStringCanopyPairGenerator[N <: Node[_] with NodeStringCanopization] extends ContextGenerator[N, (N, N)] {

  def nextContext:(N, N) = {
    if(mentions.size == 1) {throw new Error("Cannot sample pairs from a single node")}

    //println("Sampled node: %s, depth of %d, is mention: %s".format(sampleNode, sampleDepth, sampleNode.isMention))
    val m1 = mentions.sampleUniformly

    var n2 = m1.membersOfCanopies.sampleUniformly.sampleUniformly
    var sampleDepth1 = random.nextInt(m1.depth)
    val n1 = m1.ancestor(sampleDepth1)
    //if(!(n1 eq m1)) {
    //  println("1st Sampled node: %s, total depth: %d, sample depth: %d, is mention: %s".format(n1.id, m1.depth, sampleDepth1, n1.isMention))
    //}
    sampleDepth1 = random.nextInt(n2.depth)
    n2 = n2.ancestor(random.nextInt(n2.depth)).asInstanceOf[N]
    while(n1 == n2) {
      n2 = m1.membersOfCanopies.sampleUniformly.sampleUniformly
      sampleDepth1 = random.nextInt(n2.depth)
      n2 = n2.ancestor(sampleDepth1).asInstanceOf[N]
    }
    //println("2nd Sampled node: %s, total depth: %d, sample depth: %d, is mention: %s".format(n2.id, n2.depth, sampleDepth1, n2.isMention))
    n1.asInstanceOf[N] -> n2.asInstanceOf[N]
  }
}