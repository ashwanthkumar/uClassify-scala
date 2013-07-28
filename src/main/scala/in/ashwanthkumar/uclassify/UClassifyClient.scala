package in.ashwanthkumar.uclassify

import dispatch._
import dispatch.Defaults._
import scala.xml.Elem
import ResponseTransformer._

class UClassifyClient {

  val requestBuilder = RequestBuilder()

  import UClassifyClient.BASE_REQUEST

  def classify(classifier: String, textsToClassify: List[String], classifierUsername: Option[String]) = {
    val classifyXml = requestBuilder.classify(classifier, textsToClassify, classifierUsername)
    sendRequest(classifyXml.mkString) map transformClassifyResult(textsToClassify)
  }

  private[uclassify] def sendRequest(xmlStringToSend: String): dispatch.Future[Elem] = {
    val request = BASE_REQUEST << xmlStringToSend
    Http(request OK as.xml.Elem)
  }

}

object UClassifyClient {
  def BASE_REQUEST = url("http://api.uclassify.com").POST
}