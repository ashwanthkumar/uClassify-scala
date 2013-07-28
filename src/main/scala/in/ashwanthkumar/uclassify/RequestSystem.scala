package in.ashwanthkumar.uclassify

import scala.xml.{XML, Elem}
import com.typesafe.config.ConfigFactory
import scala.util.Random

case class APIInfo(readKey: String, writeKey: String)

case class TextBase64(id: String, text: String) {
  def toXml = <textBase64 id={id}>{Utils.base64Encode(text)}</textBase64>

  override def toString = toXml.mkString
}

class RequestSystem(apiInfo: APIInfo)(baseXml: Elem) {

  def classify(classifier: String, textsToClassify: List[String], classifierUsername: Option[String] = None) = {
    val defaultClassifyAttributes = Map(
      "id" -> s"classify${Random.nextInt()}",
      "classifierName" -> classifier
    )

    val classifyAttributes = if (classifierUsername
                                 .isDefined) defaultClassifyAttributes + ("username" -> classifierUsername.get)
    else defaultClassifyAttributes
    val readCalls = readCallsBuilder("classifier", classifyAttributes, Some(textsToClassify.length))

    baseXml.copy(child = baseXml.child ++ textsBuilder(textsToClassify) ++ readCalls)
  }

  def classifyKeywords(classifier: String, textsToClassify: List[String], classifierUsername: Option[String] = None) = {
    val defaultAttributes = Map(
      "id" -> s"classifyKeyWords${Random.nextInt()}",
      "classifierName" -> classifier
    )

    val classifyAttributes = if (classifierUsername
                                 .isDefined) defaultAttributes + ("username" -> classifierUsername.get)
    else defaultAttributes
    val readCalls = readCallsBuilder("classifyKeywords", classifyAttributes, Some(textsToClassify.length))

    baseXml.copy(child = baseXml.child ++ textsBuilder(textsToClassify) ++ readCalls)
  }

  def getInformation(classifier: String) = {
    val defaultAttributes = Map(
      "id" -> s"getInformation${Random.nextInt()}",
      "classifierName" -> classifier
    )

    val readCalls = readCallsBuilder("getInformation", defaultAttributes)
    baseXml.copy(child = baseXml.child ++ readCalls)
  }

  def createClassifier(classifier: String) = {
    val defaultAttributes = Map(
      "id" -> s"createClassifier${Random.nextInt()}"
    )
    val writeCalls = writeCallsBuilder("create", defaultAttributes, classifier)
    baseXml.copy(child = baseXml.child ++ writeCalls)
  }

  def removeClassifier(classifier: String) = {
    val defaultAttributes = Map(
      "id" -> s"removeClassifier${Random.nextInt()}"
    )
    val writeCalls = writeCallsBuilder("remove", defaultAttributes, classifier)
    baseXml.copy(child = baseXml.child ++ writeCalls)
  }

  def addClass(className: String, classifier: String) = {
    val defaultAttributes = Map(
      "id" -> s"addClass${Random.nextInt()}",
      "className" -> className
    )
    val writeCalls = writeCallsBuilder("addClass", defaultAttributes, classifier)
    baseXml.copy(child = baseXml.child ++ writeCalls)
  }

  def removeClass(className: String, classifier: String) = {
    val defaultAttributes = Map(
      "id" -> s"removeClass${Random.nextInt()}",
      "className" -> className
    )
    val writeCalls = writeCallsBuilder("removeClass", defaultAttributes, classifier)
    baseXml.copy(child = baseXml.child ++ writeCalls)
  }

  def train(textsToTrain: List[String], classifierClass: String, classifier: String) = {
    val defaultAttributes = Map(
      "id" -> s"trainTexts${Random.nextInt()}",
      "className" -> classifierClass
    )

    val writeCalls = writeCallsBuilder("train", defaultAttributes, classifier, Some(textsToTrain.length))

    baseXml.copy(child = baseXml.child ++ textsBuilder(textsToTrain) ++ writeCalls)
  }

  def untrain(textsToTrain: List[String], classifierClass: String, classifier: String) = {
    val defaultAttributes = Map(
      "id" -> s"untrainTexts${Random.nextInt()}",
      "className" -> classifierClass
    )

    val writeCalls = writeCallsBuilder("untrain", defaultAttributes, classifier, Some(textsToTrain.length))

    baseXml.copy(child = baseXml.child ++ textsBuilder(textsToTrain) ++ writeCalls)
  }

  private def writeCallsBuilder(operation: String, attributes: Map[String, String], classifierName: String, count: Option[Int] = None) = {
    val writeCalls = <writeCalls writeApiKey={apiInfo.writeKey} classifierName={classifierName}></writeCalls>
    val elementAttributes = attributes.map(t => t._1 + "=\"" + t._2 + "\"").mkString(" ")

    val operationXml = count match {
      case Some(size) => (1 to size).foldLeft(List[Elem]())((sofar, counter) => {
        sofar ++ List(XML.loadString(s"<$operation $elementAttributes textId='text$counter' />"))
      })

      case None => XML.loadString(s"<$operation $elementAttributes />")
    }

    writeCalls.copy(child = writeCalls.child ++ operationXml)
  }

  private def readCallsBuilder(operation: String, attributes: Map[String, String], count: Option[Int] = None) = {
    val readCalls = <readCalls readApiKey={apiInfo.readKey}></readCalls>
    val elementAttributes = attributes.map(t => t._1 + "=\"" + t._2 + "\"").mkString(" ")

    val operationXml = count match {
      case Some(size) => (1 to size).foldLeft(List[Elem]())((sofar, counter) => {
        sofar ++ List(XML.loadString(s"<$operation $elementAttributes textId='text$counter' />"))
      })

      case None => XML.loadString(s"<$operation $elementAttributes />")
    }

    readCalls.copy(child = readCalls.child ++ operationXml)
  }

  private def textsBuilder(texts: List[String]) = texts.foldLeft(<texts></texts>)((textSoFar, text) => {
    textSoFar.copy(child = textSoFar.child ++ TextBase64("text" + textSoFar.size, text).toXml)
  })
}

object RequestSystem {
  val baseClassifyRequest = <uclassify xmlns = "http://api.uclassify.com/1/RequestSchema" version = "1.01" ></uclassify>

  def apply() = {
    val config = ConfigFactory.load().getConfig("uclassify")
    new RequestSystem(APIInfo(config.getString("read-key"), config.getString("write-key")))(baseClassifyRequest)
  }

  def apply(apiInfo: APIInfo) = new RequestSystem(apiInfo)(baseClassifyRequest)
}