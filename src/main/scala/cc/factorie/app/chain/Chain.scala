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

import scala.util.Random
import scala.collection.mutable
import cc.factorie._
import cc.factorie.optimize.{OnlineTrainer, LinearMultiClassClassifier}
import scala.io.Source

object Chain {
  def main(args: Array[String]): Unit = {
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val writeSequences = new CmdOption("write-sequences", "sequences", "FILE", "Filename in which to save the sequences' labels and features.")

      // provide either these 4
      val readSequences = new CmdOption("read-sequences", "sequences", "FILE", "Filename from which to read the sequences' labels and features in one-line-per-token format.")
      val trainingPortion = new CmdOption("training-portion", 0.5, "FRACTION", "The fraction of the sequences that should be used for training.  testing-portion is 1.0 - training-portion - validation-portion.")
      //val validationPortion = new CmdOption("validation-portion", 0.0, "FRACTION", "The fraction of the sequences that should be used for validation") todo YAGNI
      val crossValidation = new CmdOption("cross-validation", 1, "N", "The number of folds for cross-validation (DEFAULT=1)")

      // or these 3
      val readTrainingSequences = new CmdOption("read-training-sequences", "sequences", "FILE", "Filename from which to read the training sequences' labels and features.")
      //val readValidationSequences = new CmdOption("read-validation-sequences", "sequences", "FILE", "Filename from which to read the validation sequences' labels and features.") todo YAGNI
      val readTestingSequences = new CmdOption("read-testing-sequences", "sequences", "FILE", "Filename from which to read the testing sequences' labels and features.")

      val readBinaryFeatures = new CmdOption("read-binary-features", true, "true|false", "If true, features will be binary as opposed to counts.  Default is true.")

      /* Probably not necessary - copied from classifier - Jack
      val readTextFiles = new CmdOption("read-text-files", Seq("textfile"), "FILE.txt...", "Files from which to read sequences; tokens become features; directory name is the label.")
      val readTextLines = new CmdOption("read-text-lines", "textfile", "FILE.txt", "Filename from which to read the sequences' labels and features; first word is the label value")
      val readTextTokenSegmenter = new CmdOption("read-text-token-segmenter", "cc.factorie.app.nlp.segment.Tokenizer", "StringSegmenter", "Scala expression providing a subclass of StringSegmenter to turn text into tokens.")
      val readTextSkipHeader = new CmdOption("read-text-skip-header", false, "true|false", "Skip text up until double newline.")
      val readTextSkipHTML = new CmdOption("read-text-skip-html", false, "true|false", "Exclude HTML tags.")
      */

      val readTextEncoding = new CmdOption("read-text-encoding", "UTF-8", "ENCODING", "The name of the encoding to use, e.g. UTF-8.")

      val writeSGML = new CmdOption("write-sgml", "output.sgml", "FILE.sgml", "File in which to write the inferred output, with SGML markup.")
      val writeOWPL = new CmdOption("write-owpl", "output.owpl", "FILE.owpl", "File in which to write the inferred output, with one-word-per-line (e.g. CoNLL-2003 format).")

      /* probably copied from classify - Jack
      val writeVocabulary = new CmdOption("write-vocabulary", "vocabulary", "FILE", "Filename in which to save the vocabulary.")
      val readVocabulary = new CmdOption("read-vocabulary", "vocabulary", "FILE", "Filename from which to load the vocabulary.")
      */

      val writeClassifications = new CmdOption("write-classifications", "classifications", "FILE", "Filename in which to save the classifications.")

      val writeChainModel = new CmdOption("write-chain-model", "chain-model", "FILE", "Filename in which to save the chain model.")
      val readChainModel = new CmdOption("read-chain-model", "chain-model", "FILE", "Filename from which to read the chain model.")
      val localRandomSeed = new CmdOption("random-seed", -1, "N", "The random seed for randomly selecting a proportion of the instance list for training")

      val trainer = new CmdOption("trainer", "ChainLikelihoodTrainer", "ChainTrainer", "Scala expression providing ChainTrainer class.")
      // TODO Consider enabling the system to use multiple ChainTrainers at the same time, and compare results

      val evaluator = new CmdOption("evaluator", "Trial", "Class()", "The constructor for a ClassifierEvaluator class.")
      val printInfoGain = new CmdOption("print-infogain", false, "true|false", "Print the training features with highest information gain.")
    }
    opts.parse(args)


    object FeaturesDomain extends CategoricalVectorDomain[String]
    object LabelDomain extends CategoricalDomain[String]



    class FeatureChain extends Chain[FeatureChain, Features]

    trait Features extends VectorVar with Observation[Features] with ChainLink[Features, FeatureChain] {
      var label:Label
      val string = "N/A"
      val domain: VectorDomain = FeaturesDomain
    }

    object Features {
      def apply(features:Iterable[String], label:Label)(implicit chain:FeatureChain):Features = opts.readBinaryFeatures.value match {
        case true => new BinaryFeatures(features, label)
        case false => new NonBinaryFeatures(features, label)
      }
    }

    class BinaryFeatures(features:Iterable[String], var label:Label) extends BinaryFeatureVectorVariable[String](features) with Features {
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
      val token = features
      override def domain = LabelDomain
    }

    /*
    def fileNametoString(fname:String):String = new File(fname) match {
      case f if f.exists() && f.isFile => Source.fromFile(f, opts.readTextEncoding.value).mkString
      case nF => throw new IllegalArgumentException("File " + nF.getName + " does not exist.")
    }
    */



    /*
    def processInstances(filename:String):Iterable[Label] = Source.fromFile(filename).getLines().map{ line =>
      line.split("\\s+").toList match {
        case labelString :: featureStrings => new Label(labelString)
        case Array("") => {} // indicates an em
        case _ => throw new Exception("Error in input format") //todo make this error more descriptive
      }
    }.toIterable

    def pInst(filename:String) = {
      val lines:List[String] = Source.fromFile(filename).getLines().toList

      for(idex <- 0 until lines.size;
        current:String <- lines(idex);
        nextOpt:Option[String] <- if(lines.size > idex + 1) Some(lines(idex + 1)) else None;
        prevOpt:Option[String] <- if(0 > idex - 1) None else Some(lines(idex - 1))) {


      }
    }
    */
    def processInstances(filename:String):Iterable[FeatureChain] = {
      val res = mutable.ArrayBuffer[FeatureChain]()
      Source.fromFile(filename).getLines().toList.map{line =>
        var chain = new FeatureChain()
        line.split("\\s+").toList match {
          case labelString :: featureStrings => { //add to label chain
            chain += Features(featureStrings, new Label(labelString))
          }
          case List("") => { //indicates the end of a chain
            res += chain
            chain = new FeatureChain()
          }
          case _ => throw new Exception("Error in input format") //todo make this error more descriptive
        }
      }
      res.toSeq
    }


    /*
    def readInstancesFromFilename(filename:String):mutable.ArrayBuffer[Label] = new File(filename) match {
      case f if f.exists() && f.isFile => {
        val cubbie = new LabelListCubbie(FeaturesDomain, LabelDomain, opts.readBinaryFeatures.value)
        BinarySerializer.deserialize(cubbie, f)
        cubbie.fetch()
      }
      case nF => throw new IllegalArgumentException("File " + nF.getName + " does not exist.")
    }
    */

    // local random seed
    implicit val random = new Random(opts.localRandomSeed.value)

    val model = new ChainModel[Label, Features, Features](LabelDomain, FeaturesDomain, _.features, _.token, _.label)

    val trainer = new OnlineTrainer()



    val trainingLabels = mutable.ArrayBuffer[Label]()
    val testingLabels = mutable.ArrayBuffer[Label]()
    val validationLabels = mutable.ArrayBuffer[Label]()


    if(Seq(opts.readSequences, opts.trainingPortion, opts.validationPortion).map(_.wasInvoked).reduce(_ && _)) {
      // read readSeq into features and do training test split
      val labels = processInstances(opts.readSequences.value)
      val (trainSet, testAndValidationSet) =
        if (opts.trainingPortion == 1.0) (labels, Seq(): Seq[Label]) else labels.shuffle.split(opts.trainingPortion.value)
      val (valSet, testSet) =
        if (opts.validationPortion.value == 0.0) (Seq(): Seq[Label], testAndValidationSet)
        else testAndValidationSet.split(opts.validationPortion.value / (1 - opts.trainingPortion.value))
      trainingLabels ++= trainSet
      testingLabels ++= testSet
      validationLabels ++= valSet

    } else if(Seq(opts.readTrainingSequences, opts.readTestingSequences, opts.readValidationSequences).map(_.wasInvoked).reduce(_ && _)) {
      trainingLabels ++= processInstances(opts.readTrainingSequences.value)
      testingLabels ++= processInstances(opts.readTestingSequences.value)
      validationLabels ++= processInstances(opts.readValidationSequences.value)
    } else {
      throw new IllegalArgumentException("Check yo args") //todo fix this message
    }

    val classifier:LinearMultiClassClassifier = trainer.train(trainingLabels, trainingLabels.map{_.features})

    //val trainer = opts.trainer.value






  }
}