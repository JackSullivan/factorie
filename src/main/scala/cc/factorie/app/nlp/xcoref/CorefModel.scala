package cc.factorie.app.nlp.xcoref

import cc.factorie.model.{TemplateModel, Parameters}

/**
 * @author John Sullivan
 */
abstract class CorefModel[Vars <: NodeVariables[Vars]] extends TemplateModel with Parameters { // This is to ensure that the model's features' NodeVariables match the type of the model's NodeVariables
  implicit val params:Parameters = this
}