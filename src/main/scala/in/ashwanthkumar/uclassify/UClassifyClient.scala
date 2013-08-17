package in.ashwanthkumar.uclassify

import dispatch._
import dispatch.Defaults._
import scala.xml.Elem
import ResponseTransformer._

class UClassifyClient(requestBuilder: RequestBuilder) {


  import UClassifyClient.BASE_REQUEST

  def classify(classifier: String, textsToClassify: List[String], classifierUsername: Option[String] = None) = {
    val classifyXml = requestBuilder.classify(classifier, textsToClassify, classifierUsername)
    sendRequest(classifyXml.mkString) map transformClassifyResult(textsToClassify)
  }

  def getInformation(classifier: String) = {
    val getInformationXml = requestBuilder.getInformation(classifier)
    sendRequest(getInformationXml.mkString) map transformGetInformationResult
  }

  def createClassifier(classifier: String) = {
    val createClassifier = requestBuilder.createClassifier(classifier)
    sendRequest(createClassifier.mkString) map verifyResponse
  }

  def removeClassifier(classifier: String) = {
    val removeClassifier = requestBuilder.removeClassifier(classifier)
    sendRequest(removeClassifier.mkString) map verifyResponse
  }

  def addClass(className: String, classifier: String) = {
    val addClassToClassifier = requestBuilder.addClass(className, classifier)
    sendRequest(addClassToClassifier.mkString) map verifyResponse
  }

  def removeClass(className: String, classifier: String) = {
    val removeClassFromClassifier = requestBuilder.removeClass(className, classifier)
    sendRequest(removeClassFromClassifier.mkString) map verifyResponse
  }

  def train(textsToTrain: List[String], classifierClass: String, classifier: String) = {
    val trainClassifier = requestBuilder.train(textsToTrain, classifierClass, classifier)
    sendRequest(trainClassifier.mkString) map verifyResponse
  }

  def untrain(textsToUntrain: List[String], classifierClass: String, classifier: String) = {
    val untrainClassifier = requestBuilder.untrain(textsToUntrain, classifierClass, classifier)
    sendRequest(untrainClassifier.mkString) map verifyResponse
  }

  private[uclassify] def sendRequest(xmlStringToSend: String): dispatch.Future[Elem] = {
    val request = BASE_REQUEST << xmlStringToSend
    Http(request OK as.xml.Elem)
  }

}

object UClassifyClient {
  def BASE_REQUEST = url("http://api.uclassify.com").POST

  def apply() = new UClassifyClient(RequestBuilder())

  def apply(readApiKey: String, writeApiKey: String) = new UClassifyClient(
    RequestBuilder(
      APIInfo(readApiKey,writeApiKey)
    ))
}