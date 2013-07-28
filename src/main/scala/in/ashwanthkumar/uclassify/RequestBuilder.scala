package in.ashwanthkumar.uclassify

import scala.xml.{XML, Elem}
import com.typesafe.config.ConfigFactory
import scala.util.Random

case class APIInfo(readKey: String, writeKey: String)

case class TextBase64(id: String, text: String) {
  def toXml = <textBase64 id={id}>{Utils.base64Encode(text)}</textBase64>

  override def toString = toXml.mkString
}

class RequestBuilder(apiInfo: APIInfo)(baseXml: Elem) {

  def classify(classifier: String, textsToClassify: List[String], classifierUsername: Option[String] = None) = {
    val defaultClassifyAttributes = Map(
      "classifierName" -> classifier
    )

    val classifyAttributes = if (classifierUsername
                                 .isDefined) defaultClassifyAttributes + ("username" -> classifierUsername.get)
    else defaultClassifyAttributes
    val readCalls = readCallsBuilder("classify", classifyAttributes, Some(textsToClassify.length))

    baseXml.copy(child = baseXml.child ++ textsBuilder(textsToClassify) ++ readCalls)
  }

  def classifyKeywords(classifier: String, textsToClassify: List[String], classifierUsername: Option[String] = None) = {
    val defaultAttributes = Map(
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
      "classifierName" -> classifier
    )

    val readCalls = readCallsBuilder("getInformation", defaultAttributes)
    baseXml.copy(child = baseXml.child ++ readCalls)
  }

  def createClassifier(classifier: String) = {
    val writeCalls = writeCallsBuilder("create", Map(), classifier)
    baseXml.copy(child = baseXml.child ++ writeCalls)
  }

  def removeClassifier(classifier: String) = {
    val writeCalls = writeCallsBuilder("remove", Map(), classifier)
    baseXml.copy(child = baseXml.child ++ writeCalls)
  }

  def addClass(className: String, classifier: String) = {
    val defaultAttributes = Map(
      "className" -> className
    )
    val writeCalls = writeCallsBuilder("addClass", defaultAttributes, classifier)
    baseXml.copy(child = baseXml.child ++ writeCalls)
  }

  def removeClass(className: String, classifier: String) = {
    val defaultAttributes = Map(
      "className" -> className
    )
    val writeCalls = writeCallsBuilder("removeClass", defaultAttributes, classifier)
    baseXml.copy(child = baseXml.child ++ writeCalls)
  }

  def train(textsToTrain: List[String], classifierClass: String, classifier: String) = {
    val defaultAttributes = Map(
      "className" -> classifierClass
    )

    val writeCalls = writeCallsBuilder("train", defaultAttributes, classifier, Some(textsToTrain.length))
    baseXml.copy(child = baseXml.child ++ textsBuilder(textsToTrain) ++ writeCalls)
  }

  def untrain(textsToTrain: List[String], classifierClass: String, classifier: String) = {
    val defaultAttributes = Map(
      "className" -> classifierClass
    )

    val writeCalls = writeCallsBuilder("untrain", defaultAttributes, classifier, Some(textsToTrain.length))
    baseXml.copy(child = baseXml.child ++ textsBuilder(textsToTrain) ++ writeCalls)
  }

  private def writeCallsBuilder(operation: String, attributes: Map[String, String], classifierName: String, count: Option[Int] = None) = {
    val writeCalls = <writeCalls writeApiKey={apiInfo.writeKey} classifierName={classifierName}></writeCalls>
    def elementAttributes(counter: Int) = (attributes ++ Map("id" -> s"$operation$counter"))
                                        .map(t => t._1 + "=\"" + t._2 + "\"")
                                        .mkString(" ")

    val operationXml = count match {
      case Some(size) => (1 to size).foldLeft(List[Elem]())((sofar, counter) => {
        sofar ++ List(XML.loadString(s"<$operation ${elementAttributes(counter)} textId='text$counter' />"))
      })

      case None => XML.loadString(s"<$operation ${elementAttributes(0)} />")
    }

    writeCalls.copy(child = writeCalls.child ++ operationXml)
  }

  private def readCallsBuilder(operation: String, attributes: Map[String, String], count: Option[Int] = None) = {
    val readCalls = <readCalls readApiKey={apiInfo.readKey}></readCalls>
    def elementAttributes(counter: Int) = (attributes ++ Map("id" -> s"$operation$counter"))
                                          .map(t => t._1 + "=\"" + t._2 + "\"")
                                          .mkString(" ")

    val operationXml = count match {
      case Some(size) => (1 to size).foldLeft(List[Elem]())((sofar, counter) => {
        sofar ++ List(XML.loadString(s"<$operation ${elementAttributes(counter - 1)} textId='text$counter' />"))
      })

      case None => XML.loadString(s"<$operation ${elementAttributes(0)} />")
    }

    readCalls.copy(child = readCalls.child ++ operationXml)
  }

  private def textsBuilder(texts: List[String]) = texts.foldLeft(<texts></texts>)((textSoFar, text) => {
    textSoFar.copy(child = textSoFar.child ++ TextBase64("text" + (textSoFar.child.size + 1), text).toXml)
  })
}

object RequestBuilder {
  val baseClassifyRequest = <uclassify xmlns = "http://api.uclassify.com/1/RequestSchema" version = "1.01" ></uclassify>

  def apply() = {
    val config = ConfigFactory.load().getConfig("uclassify")
    new RequestBuilder(APIInfo(config.getString("read-key"), config.getString("write-key")))(baseClassifyRequest)
  }

  def apply(apiInfo: APIInfo) = new RequestBuilder(apiInfo)(baseClassifyRequest)
}