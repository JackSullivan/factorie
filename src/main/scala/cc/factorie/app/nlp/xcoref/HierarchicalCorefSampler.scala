package cc.factorie.app.nlp.xcoref

/**
 * @author John Sullivan
 */
abstract class HierarchicalCorefSampler[Vars <: NodeVariables[Vars], N <: Node[Vars] with NodeStringCanopization](model :CorefModel[Vars], mentions:Iterable[N], iterations:Int)
  extends CorefSampler[Vars, N](model, mentions, iterations)
  with LegacyPairGenerator[N]
  with LegacyMoveGenerator[Vars, N]
