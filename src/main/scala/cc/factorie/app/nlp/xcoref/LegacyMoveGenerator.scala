package cc.factorie.app.nlp.xcoref

import cc.factorie.infer.SettingsSampler
import scala.collection.mutable.ArrayBuffer
import cc.factorie.variable.{SettingIterator, DiffList}

/**
 * @author John Sullivan
 */
trait LegacyMoveGenerator[Vars <: NodeVariables[Vars], N <: Node[Vars]] extends MoveGenerator[Vars, N] {
  this: SettingsSampler[(N, N)] =>


  protected def expandedContext(context: (N, N)): Iterable[(N, N)] = List(context)

  def moves: IndexedSeq[Move[N]] = Vector.empty[Move[N]]

  def mergeLeft(left:N, right:N)(d:DiffList) {
    right.alterParent(Some(left))(d)
  }
  def mergeUp(left:N, right:N)(d:DiffList) {
    val n = newInstance(d)
    left.alterParent(Some(n))(d)
    right.alterParent(Some(n))(d)
  }
  def splitRight(left:N, right:N)(d:DiffList) {
    right.alterParent(None)(d)
  }

  def innerProposeMergeIfValid(entity1:N,entity2:N,changes:ArrayBuffer[(DiffList)=>Unit]):Unit ={
    //only do the merge if the entities are in different trees, and further require that a mention never has children (always a leaf).
    if (entity1.root.id != entity2.root.id){ //sampled nodes refer to different entities
      if(!entity1.isMention)changes += {(d:DiffList) => mergeLeft(entity1,entity2)(d)}
      else if(!entity2.isMention)changes += {(d:DiffList) => mergeLeft(entity2,entity1)(d)}
    }
  }

  def proposeMergeIfValid(entity1:N,entity2:N,changes:ArrayBuffer[(DiffList)=>Unit]):Unit ={
    // println("Propose Merge Up if Valid!")
    if(entity1.mentionCountVar.value>=entity2.mentionCountVar.value)
      innerProposeMergeIfValid(entity1,entity2,changes)
    else
      innerProposeMergeIfValid(entity2,entity1,changes)
  }

  override def settings(c:(N, N)) : SettingIterator = new SettingIterator {
    val changes = new scala.collection.mutable.ArrayBuffer[(DiffList)=>Unit]
    val (entityS1,entityS2) = c
    val entity1 = entityS1//.ancestor(random.nextInt(entityS1.depth))
    val entity2 = entityS2//.ancestor(random.nextInt(entityS2.depth))
    //if(!entity1.moveable || !entity2.moveable || !entityS1.moveable || !entityS2.moveable)println("WARNING: attempting to move a moveable entity.")
    //if(entity1.source=="wp" || entity2.source=="wp" || entityS1.source=="wp" || entityS2.source=="wp")println("WP MENTION")
    val r1 = entity1.root
    val r2 = entity2.root
    if (r1.id != r2.id) { //sampled nodes refer to different entities
    var e1 = entityS1
      var e2 = entity2//entityS2.getAncestor(random.nextInt(entityS2.depth+1)).asInstanceOf[KBEntity]
      /*
      if(entity1.source=="wp"){
        println("Sampling WP")
        if(!r2.isMention){changes += {(d:DiffList) =>mergeLeft(r2,entity1)(d)}}; //println("Merge Left!")}
        else {changes += {(d:DiffList) => mergeUp(entity1,r2)(d)};} //println("Merge Up!")}
      }
      if(entity2.source=="wp"){
        println("Sampling WP")
        if(!r1.isMention) {{changes += {(d:DiffList) => mergeLeft(r1,entity2)(d)}}};// println("Merge Left!")} }
        else { changes += {(d:DiffList) => mergeUp(entity2,r1)(d)}}; //println("Merge Up!") }
      }
      */
      while(e1 != null){
        proposeMergeIfValid(e1,e2.asInstanceOf[N],changes)
        e1 = e1.parent.getOrElse(null).asInstanceOf[N]
      }
      //if(!e1.eq(r1))proposeMergeIfValid(r1,r2,changes)

      if(entity1.parent.isEmpty && entity2.parent.isEmpty){
        changes += {(d:DiffList) => mergeUp(entity1.asInstanceOf[N],entity2.asInstanceOf[N])(d)}
        //println("Merge Up!")
      }
      if(changes.size==0){
        if(!r2.isMention){
          proposeMergeIfValid(r2.asInstanceOf[N],entity1.asInstanceOf[N],changes)
          if(!r1.eq(entity1))proposeMergeIfValid(r2.asInstanceOf[N],r1.asInstanceOf[N],changes)
        } else{
          //val r1 = entity1.entityRoot.asInstanceOf[KBEntity]
          if(!r1.isMention){
            proposeMergeIfValid(r1.asInstanceOf[N],entity2.asInstanceOf[N],changes)
          }else {changes += {
            (d:DiffList) => mergeUp(entity1.root.asInstanceOf[N],entity2.root.asInstanceOf[N])(d)
          };} //println("Merge up!")}
        }
      }
    } else { //sampled nodes refer to same entity
      if(entity1.mentionCountVar.value>entity2.mentionCountVar.value) {
        changes += {(d:DiffList) => splitRight(entity2.asInstanceOf[N],entity1.asInstanceOf[N])(d)}
      }
      else {
        changes += {(d:DiffList) => splitRight(entity1.asInstanceOf[N],entity2.asInstanceOf[N])(d)}
      }
    }

    changes += {(d:DiffList) => {}} //give the sampler an option to reject all other proposals
    var i = 0
    def hasNext = i < changes.length
    def next(d:DiffList) = {val d = newDiffList; changes(i).apply(d); i += 1; d}
    def reset = i = 0
  }
}
