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

  private def readCallsBuilder(operation: String, attributes: Map[String, String], count: Option[Int] = Some(1)) = {
    val readCalls = <readCalls readApiKey={apiInfo.readKey}></readCalls>
    val elementAttributes = attributes.map(t => t._1 + "=\"" + t._2 + "\"").mkString(" ")

    val operationXml = (1 to count.get).foldLeft(List[Elem]())((sofar, counter) => {
      sofar ++ List(XML.loadString(s"<$operation $elementAttributes textId='text$counter' />"))
    })

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