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

package cc.factorie.app.chain

import java.io.FileOutputStream
import scala.util.Random
import scala.collection.mutable
import cc.factorie._
import cc.factorie.optimize.{LikelihoodExample, OnlineTrainer}
import cc.factorie.util.DefaultCmdOptions
import scala.io.Source
import scala.annotation.tailrec

object ChainOpts extends DefaultCmdOptions {

  val writeSequences = new CmdOption("write-sequences", "sequences", "FILE", "Filename in which to save the sequences' labels and features.")

  // provide either these 3
  val readSequences = new CmdOption("read-sequences", "sequences", "FILE", "Filename from which to read the sequences' labels and features in one-line-per-token format.")
  val trainingPortion = new CmdOption("training-portion", 0.5, "FRACTION", "The fraction of the sequences that should be used for training.  testing-portion is 1.0 - training-portion - validation-portion.")
  val crossValidation = new CmdOption("cross-validation", 1, "N", "The number of folds for cross-validation (DEFAULT=1)")

  // or these 2
  val readTrainingSequences = new CmdOption("read-training-sequences", "sequences", "FILE", "Filename from which to read the training sequences' labels and features.")
  val readTestingSequences = new CmdOption("read-testing-sequences", "sequences", "FILE", "Filename from which to read the testing sequences' labels and features.")

  val readBinaryFeatures = new CmdOption("read-binary-features", true, "true|false", "If true, features will be binary as opposed to counts.  Default is true.")

  val readTextEncoding = new CmdOption("read-text-encoding", "UTF-8", "ENCODING", "The name of the encoding to use, e.g. UTF-8.")

  val writeClassifications = new CmdOption("write-classifications", "classifications", "FILE", "Filename in which to save the classifications.") //todo implement this CLA

  val writeChainModel = new CmdOption("write-chain-model", "chain-model", "FILE", "Filename in which to save the chain model.")
  val readChainModel = new CmdOption("read-chain-model", "chain-model", "FILE", "Filename from which to read the chain model.") //todo implement this CLA

  val localRandomSeed = new CmdOption("random-seed", -1, "N", "The random seed for randomly selecting a proportion of the instance list for training")

  val trainer = new CmdOption("trainer", "ChainLikelihoodTrainer", "ChainTrainer", "Scala expression providing ChainTrainer class.") //todo implement this CLA
  // TODO Consider enabling the system to use multiple ChainTrainers at the same time, and compare results

  val evaluator = new CmdOption("evaluator", "Trial", "Class()", "The constructor for a ClassifierEvaluator class.") //todo implement this CLA
  val printInfoGain = new CmdOption("print-infogain", false, "true|false", "Print the training features with highest information gain.") // todo implement and share across this and classify
}

object FeaturesDomain extends CategoricalVectorDomain[String]
object LabelDomain extends CategoricalDomain[String]

class FeatureChain extends Chain[FeatureChain, Features]

trait Features extends CategoricalVectorVar[String] with Observation[Features] with ChainLink[Features, FeatureChain] {
  var label:Label
  val string = "N/A"
  override def domain = FeaturesDomain
}

object Features {
  def apply(features:Iterable[String], label:Label):Features = ChainOpts.readBinaryFeatures.value match {
    case true => new BinaryFeatures(features, label)
    case false => new NonBinaryFeatures(features, label)
  }
}

class BinaryFeatures(features:Iterable[String], var label:Label) extends BinaryFeatureVectorVariable[String](features) with Features {
  //println("Making binary with features: %s".format(features.mkString("|")))
  label.featOpt = Some(this)
}
class NonBinaryFeatures(features:Iterable[String], var label:Label) extends FeatureVectorVariable[String](features) with Features {
  label.featOpt = Some(this)
}

class Label(value:String) extends LabeledCategoricalVariable[String](value) {
  var featOpt:Option[Features] = None
  def features:Features = featOpt match {
    case Some(feature) => feature
    case None => throw new Exception("Uninitialized Features")
  }
  def token = features
  override def domain = LabelDomain
}

object Chain {






  def main(args: Array[String]): Unit = {

    val startTime = System.currentTimeMillis()

    
    ChainOpts.parse(args)

    // local random seed
    implicit val random = new Random(ChainOpts.localRandomSeed.value)

    def processInstances(filename:String):Iterable[FeatureChain] = {
      val src = Source.fromFile(filename)

      val featureChains =  src.getLines().toSeq.map(_.split("\\s+").toList).split(_.nonEmpty).map{ chains =>
        new FeatureChain() ++= chains.collect{ case labelString :: featureString =>
          Features(featureString, new Label(labelString))
        }.toIterable
      }
      src.close()
      featureChains
    }
    /*
    def processInstances(filename:String):Iterable[FeatureChain] = {
      val res = mutable.ArrayBuffer[FeatureChain]()
      Source.fromFile(filename).getLines().toList.map{line =>
        var chain = new FeatureChain()
        line.split("\\s+").toList match {
          case labelString :: featureStrings => { //add to label chain
            //println("creating features of size %s".format(featureStrings.size))
            val f = Features(featureStrings, new Label(labelString))
            println("Adding feature: %s, size is: %d".format(f, chain.size))
            chain += f
            println("After addition, size is: %d" format chain.size)
          }
          case l if l.isEmpty => { //indicates the end of a chain
            println("reseting list: %s" format l.mkString("|"))
            chain.chainFreeze
            res += chain
            chain = new FeatureChain()
          }
          case other => throw new Exception("Error in input format found: %s" format other) //todo make this error more descriptive
        }
      }
      //println("created chains: %s" format res.map{_.size}.mkString("\n"))
      res.toSeq
    }
    */
    /* todo remove before completion
    println(ChainOpts.readTrainingSequences.value)
    println(ChainOpts.readTestingSequences.value)
    if(ChainOpts.readTrainingSequences.wasInvoked) println("we invoked training")
    if(ChainOpts.readTestingSequences.wasInvoked) println("we invoked testing")
    */

    // todo share across this and classify
    val (trainingLabels, testingLabels) = if(Seq(ChainOpts.readSequences, ChainOpts.trainingPortion).map(_.wasInvoked).reduce(_ && _)) {
      processInstances(ChainOpts.readSequences.value) match {
        case labels if ChainOpts.trainingPortion == 1.0 => labels -> Seq()
        case labels => labels.shuffle.split(ChainOpts.trainingPortion.value)
      }
    } else if(Seq(ChainOpts.readTrainingSequences, ChainOpts.readTestingSequences).map(_.wasInvoked).reduce(_ && _)) {
      processInstances(ChainOpts.readTrainingSequences.value) -> processInstances(ChainOpts.readTestingSequences.value)
    } else {
      throw new IllegalArgumentException("Check yo args") //todo fix this message
    }

    val model = new ChainModel[Label, Features, Features](LabelDomain, FeaturesDomain, _.features, _.token, _.label)

    //val examples = trainingLabels.flatMap{_.map{f:Features => new LikelihoodExample(f.label, model, InferByBPChainSum)}}
    val examples = trainingLabels.map{ fc => new LikelihoodExample(fc.map{f:Features => f.label}, model, InferByBPChainSum)}

    val trainer = new OnlineTrainer(model.parameters, maxIterations = 1)

    trainer.trainFromExamples(examples)

    var totalTokens = 0.0
    var correctTokens = 0.0

    testingLabels.foreach{ fc =>
      MaximizeByBPChain.maximize(fc.map{f:Features => f.label}, model, null)
      totalTokens += fc.size
      correctTokens += HammingObjective.accuracy(fc.map{f:Features => f.label})
    }

    println("Overall accuracy: " + (correctTokens / totalTokens))

    println("Total elapsed time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "sec")

    // serialize the model
    if(ChainOpts.writeChainModel.wasInvoked) {
      model.serialize(new FileOutputStream(ChainOpts.writeChainModel.value))
    }


    //MaximizeByBPChain.infer(testingLabels.flatMap(_.map{f:Features => f.label}), model, null)

  }
}