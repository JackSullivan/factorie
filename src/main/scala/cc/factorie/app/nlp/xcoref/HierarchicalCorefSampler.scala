package cc.factorie.app.nlp.xcoref

import scala.util.Random

/**
 * @author John Sullivan
 */
abstract class HierarchicalCorefSampler[Vars <: NodeVariables[Vars] with Canopy](model :CorefModel[Vars], mentions:Iterable[Node[Vars]], iterations:Int)(implicit random:Random)
  extends CorefSampler[Vars](model, mentions, iterations)
  with DefaultMoveGenerator[Vars]
  with CanopyPairGenerator[Vars]
