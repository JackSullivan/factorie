package cc.factorie.app.nlp.xcoref

import cc.factorie.infer.{Proposal, SettingsSampler}
import scala.util.Random
import cc.factorie.util.Hooks1

/**
 * User:harshal, John Sullivan
 * Date: 10/28/13
 */

abstract class CorefSampler[Vars <: NodeVariables[Vars], N <: Node[Vars]](model:CorefModel[Vars], val mentions:Iterable[N], val iterations:Int)(implicit override val random:Random)
  extends SettingsSampler[(N, N)](model) {
  this: ContextGenerator[N, (N, N)] with MoveGenerator[Vars, N] =>

  this.temperature = 0.001

  //if we accepted a diff where Node#Exists variables were false at the end, we mark those nodes for deletion
  proposalHooks += {proposal:Proposal[(N, N)] =>

    proposal.diff.variables.collect{case e:Node[Vars]#Exists if !e.booleanValue => e.node.markForDeletion}
    //if (totalProps > 5000 && proposal.modelScore <= 0){
    //  println("PICKED: " + proposal.modelScore)
    //}

  }

  val beforeInferHooks = new Hooks1[Unit]
  protected def beforeInferHook = beforeInferHooks()

  def infer {
    beforeInferHook
    processAll(contexts)
  }

}
