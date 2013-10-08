package cc.factorie.app.nlp.hcoref1

import cc.factorie.variable.ArrowVariable
import scala.annotation.tailrec
import cc.factorie.app.nlp.hcoref.SparseBagOfWords

/**
 * @author harshal
 * @date: 10/3/13
 */

abstract class Node[T <: Node[T]] {
  this: T =>
  val parentRef = new ArrowVariable[T, Option[T]](this, None)
  def parent : Option[T] = parentRef.dst
  def children : Seq[T]
  def descendants : Seq[T] //dfs or bfs
  def leaves : Seq[T] = descendants.filter(_.children.isEmpty)
  private def addChild(c: T){

  }
  @tailrec
  final def root : T = parent match { case Some(p) => p.root; case None => this }

  def depth : Int = {
    def depthHelper(root: Option[T]):Int = {
      if(!parent.isDefined) 0
      else 1 + depthHelper(parent)
    }
    depthHelper(Some(this))
  }

  def size : Int  = ???

  def alterParent(p:Option[T]){
    val oldParent = parent
    val newParent = p.get
    //call hook
    p match{
      case Some(parent) => {
        newParent.addChild(this)
      }
    }
  }

  def bow[B <: SparseBagOfWords]()

  def alter(a:Any):PartialFunction[Any, Int] = {
    case a:Int => 1
  }
}


//class Mention extends Node[Mention] {
//  val children = Nil
//  override val size = 1
//  val descendants = Nil
//  override val leaves = Seq(this)
//
//  override def alter(a:Any):PartialFunction[Any, Int] = super.alter(a) orElse {
//    case _ =>  2
//  }
//
//}