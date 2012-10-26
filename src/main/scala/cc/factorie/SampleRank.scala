/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */



package cc.factorie
import cc.factorie.la._
import collection.mutable.HashMap
import optimize.{Example, GradientOptimizer}
import util.DoubleAccumulator

/** Set the parameters so that the model.score ranks the top sample the same as the objective.score, with a margin. */
class SampleRank[C](val model:Model, sampler:ProposalSampler[C], optimizer:GradientOptimizer) {
  def this(sampler:ProposalSampler[C], optimizer:GradientOptimizer) = this(sampler.model, sampler, optimizer)
  var learningMargin = 1.0
  def familiesToUpdate: Seq[DotFamily] = model.familiesOfClass(classOf[DotFamily])
  def gradientAndMargin(proposals:Seq[Proposal]): (Tensor,Double) = {
    var g: WeightsTensor = null // model.newSparseWeightsTensor
    //val g2 = new TensorMap; def gg(df:DotFamily): Tensor = g.getOrElseUpdate(df, Tensor.newSparse(df.weights)
    val bestModels = proposals.max2ByDouble(_ modelScore); val bestModel1 = bestModels._1; val bestModel = bestModels._2
    val bestObjectives = proposals.max2ByDouble(_ objectiveScore); val bestObjective1 = bestObjectives._1; val bestObjective2 = bestObjectives._2
    val margin = bestModel1.modelScore - bestModel.modelScore
    if (bestModel1 ne bestObjective1) {
      // ...update parameters by adding sufficient stats of truth, and subtracting error
      g = model.newBlankSparseWeightsTensor
      bestObjective1.diff.redo
      model.factorsOfFamilies(bestObjective1.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.currentStatistics, 1.0))
      bestObjective1.diff.undo
      model.factorsOfFamilies(bestObjective1.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.currentStatistics, -1.0))
      bestModel1.diff.redo
      model.factorsOfFamilies(bestModel1.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.currentStatistics, -1.0))
      bestModel1.diff.undo
      model.factorsOfFamilies(bestModel1.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.currentStatistics, 1.0))
    }
    else if (margin < learningMargin) {
      // ...update parameters by adding sufficient stats of truth, and subtracting runner-up
      g = model.newBlankSparseWeightsTensor
      bestObjective1.diff.redo
      model.factorsOfFamilies(bestModel1.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.currentStatistics, 1.0))
      bestObjective1.diff.undo
      model.factorsOfFamilies(bestModel1.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.currentStatistics, -1.0))
      bestModel.diff.redo
      model.factorsOfFamilies(bestModel.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.currentStatistics, -1.0))
      bestModel.diff.undo
      model.factorsOfFamilies(bestModel.diff, familiesToUpdate).foreach(f => g(f.family).+=(f.currentStatistics, 1.0))
    }
    (g, margin)
  }
  def process(c:C): DiffList = {
    val proposals = sampler.proposals(c)
    val (gradient,margin) = gradientAndMargin(proposals)
    if (gradient ne null)
      optimizer.step(model.weightsTensor, gradient, Double.NaN, margin)
    val bestProposal = proposals.maxByDouble(_.modelScore)
    bestProposal.diff.redo
    bestProposal.diff
  }
  def process(c:C, repeat:Int): Unit = for (i <- 0 until repeat) process(c)
  def processAll(cs:Iterable[C]): Unit = cs.foreach(process(_))
  def processAll(cs:Iterable[C], repeat:Int): Unit = for (i <- 0 until repeat) cs.foreach(process(_))
}

class SampleRankExample[C](val context: C, val sampler: ProposalSampler[C]) extends Example[Model] {
  var learningMargin = 1.0
  var zeroGradient = true
  def accumulateExampleInto(model: Model, gradient: WeightsTensorAccumulator, value: DoubleAccumulator, margin:DoubleAccumulator): Unit = {
    require(gradient != null, "The SampleRankExample needs a gradient accumulator")
    val familiesToUpdate: Seq[DotFamily] = model.familiesOfClass(classOf[DotFamily])
    val proposals = sampler.proposals(context)
    val (bestModel1, bestModel) = proposals.max2ByDouble(_.modelScore)
    val (bestObjective1, bestObjective2) = proposals.max2ByDouble(_.objectiveScore)
    val marg = bestModel1.modelScore - bestModel.modelScore
    zeroGradient = true
    if (bestModel1 ne bestObjective1) {
      // ...update parameters by adding sufficient stats of truth, and subtracting error
      zeroGradient = false
      bestObjective1.diff.redo
      model.factorsOfFamilies(bestObjective1.diff, familiesToUpdate).foreach(f => gradient.accumulate(f.family.asInstanceOf[DotFamily], f.currentStatistics))
      bestObjective1.diff.undo
      model.factorsOfFamilies(bestObjective1.diff, familiesToUpdate).foreach(f => gradient.accumulate(f.family, f.currentStatistics, -1.0))
      bestModel1.diff.redo
      model.factorsOfFamilies(bestModel1.diff, familiesToUpdate).foreach(f => gradient.accumulate(f.family, f.currentStatistics, -1.0))
      bestModel1.diff.undo
      model.factorsOfFamilies(bestModel1.diff, familiesToUpdate).foreach(f => gradient.accumulate(f.family, f.currentStatistics))
    }
    else if (marg < learningMargin) {
      // ...update parameters by adding sufficient stats of truth, and subtracting runner-up
      zeroGradient = false
      bestObjective1.diff.redo
      model.factorsOfFamilies(bestModel1.diff, familiesToUpdate).foreach(f => gradient.accumulate(f.family, f.currentStatistics))
      bestObjective1.diff.undo
      model.factorsOfFamilies(bestModel1.diff, familiesToUpdate).foreach(f => gradient.accumulate(f.family, f.currentStatistics, -1.0))
      bestModel.diff.redo
      model.factorsOfFamilies(bestModel.diff, familiesToUpdate).foreach(f => gradient.accumulate(f.family, f.currentStatistics, -1.0))
      bestModel.diff.undo
      model.factorsOfFamilies(bestModel.diff, familiesToUpdate).foreach(f => gradient.accumulate(f.family, f.currentStatistics))
    }
    val bestProposal = proposals.maxByDouble(_.modelScore)
    bestProposal.diff.redo
    if (margin ne null)
      margin.accumulate(marg) // TODO But this isn't really a "value", it is the "margin".  But how else to return it?
  }
}

/** A Trainer that does stochastic gradient ascent on gradients from SampleRankExamples. */
class SampleRankTrainer[C](val model:Model, sampler:ProposalSampler[C], optimizer:GradientOptimizer) extends optimize.Trainer[Model] {
  def this(sampler:ProposalSampler[C], optimizer:GradientOptimizer) = this(sampler.model, sampler, optimizer)
  val modelWeights = model.weightsTensor
  def processContext(context:C): Unit = process(new SampleRankExample(context, sampler))
  def processContexts(contexts:Iterable[C]): Unit = contexts.foreach(c => processContext(c))
  def processContexts(contexts:Iterable[C], iterations:Int): Unit = for (i <- 0 until iterations) processContexts(contexts)
  def process(example:Example[Model]): Unit = {
    val gradientAccumulator = new LocalWeightsTensorAccumulator(model.newBlankSparseWeightsTensor)
    val marginAccumulator = new util.LocalDoubleAccumulator(0.0)
    example.accumulateExampleInto(model, gradientAccumulator, null, marginAccumulator)
    // Note that here valueAccumulator will actually contain the SampleRank margin
    if (!example.asInstanceOf[SampleRankExample[C]].zeroGradient)
      optimizer.step(modelWeights, gradientAccumulator.tensor, Double.NaN, marginAccumulator.value)
  }
  def processAll(examples: Iterable[Example[Model]]): Unit = examples.foreach(p => process(p))
  def processAll(examples: Iterable[Example[Model]], iterations:Int): Unit = for (i <- 0 until iterations) processAll(examples)
}

// In the old SampleRank there was something like the following.  Do we need this for any reason?
/*
  def shouldUpdate: Boolean = {
    if (amIMetropolis) {
      val changeProposal = if (bestModel1.diff.size > 0) bestModel1 else bestModel
      !(changeProposal.modelScore * changeProposal.objectiveScore > 0 || changeProposal.objectiveScore == 0)      
    } else {
      // the objective function has some preference (e.g. we don't have an unlabeled example here)
      (bestObjective1.objectiveScore > bestObjective2.objectiveScore || bestObjective1.objectiveScore > bestModel1.objectiveScore) &&
      // the model got it wrong, or isn't confident enough about being right
      // TODO should this be based on acceptanceScore instead of modelScore?
      ((bestModel1 ne bestObjective1) || math.abs(bestModel1.modelScore - bestModel.modelScore) < learningMargin)
    }
  }

 */

