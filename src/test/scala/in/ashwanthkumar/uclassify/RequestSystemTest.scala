package in.ashwanthkumar.uclassify

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class RequestSystemTest extends FunSpec with ShouldMatchers {

  val testAPI = APIInfo("readKey", "writeKey")
  val requestSystem = RequestSystem(testAPI)

  describe("TextBase64") {
    it("should return xml on toXML") {
      TextBase64("text1", "someText").toXml should be(<textBase64 id='text1'>c29tZVRleHQ=</textBase64>)
    }
  }

  describe("Request System") {
    describe("should construct xml requests for") {
      it("classify") {
        val classifyRequest = requestSystem.classify("testClassifier", List("some text1", "some text2"))
        println(classifyRequest)

        val classifierNames = (classifyRequest \\ "classifier").map(_ \ "@classifierName")
        2 should be(classifierNames.size)
        "testClassifier" should be(classifierNames.head.mkString)

        val readApiKey = (classifyRequest \\ "readCalls").map(_ \ "@readApiKey")
        readApiKey.head.mkString should be("readKey")

        val textsBase64 = (classifyRequest \\ "textBase64").map(_.text)
        2 should be(textsBase64.size)
        textsBase64.contains(Utils.base64Encode("some text1"))
        textsBase64.contains(Utils.base64Encode("some text2"))

      }
    }
  }
}
