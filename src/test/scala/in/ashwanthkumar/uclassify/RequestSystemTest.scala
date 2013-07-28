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
      it("classifyKeywords") {
        val classifyKeywordsRequest = requestSystem.classifyKeywords("testClassifier", List("some text1", "some text2"))

        val classifierNames = (classifyKeywordsRequest \\ "classifyKeywords").map(_ \ "@classifierName")
        2 should be(classifierNames.size)
        "testClassifier" should be(classifierNames.head.mkString)

        val textsBase64 = (classifyKeywordsRequest \\ "textBase64").map(_.text)
        2 should be(textsBase64.size)
        textsBase64.contains(Utils.base64Encode("some text1"))
        textsBase64.contains(Utils.base64Encode("some text2"))
      }

      it("getInformation") {
        val getInformationRequest = requestSystem.getInformation("testClassifier")

        val classifierNames = (getInformationRequest \\ "getInformation").map(_ \ "@classifierName")
        1 should be(classifierNames.size)
        "testClassifier" should be(classifierNames.head.mkString)

        val textsBase64 = (getInformationRequest \\ "textBase64").map(_.text)
        0 should be(textsBase64.size)
      }

      it("createClassifier") {
        val createClassifierRequest = requestSystem.createClassifier("testClassifier")

        val createCalls = createClassifierRequest \\ "create"
        1 should be(createCalls.size)
        val writeApiKey = (createClassifierRequest \\ "writeCalls").map(_ \ "@writeApiKey")
        "writeKey" should be(writeApiKey.head.mkString)

        val textsBase64 = (createClassifierRequest \\ "textBase64").map(_.text)
        0 should be(textsBase64.size)
      }

      it("removeClassifier") {
        val removeClassifierRequest = requestSystem.removeClassifier("testClassifier")

        val createCalls = removeClassifierRequest \\ "remove"
        1 should be(createCalls.size)
      }

      it("addClass") {
        val addClassRequest = requestSystem.addClass("class11", "testClassifier")

        val addedClass = addClassRequest \\ "addClass"
        1 should be(addedClass.size)
        "class11" should be(addedClass.head.attribute("className").mkString)
      }

      it("removeClass") {
        val removeClassRequest = requestSystem.removeClass("class11", "testClassifier")

        val removedClass = removeClassRequest \\ "removeClass"
        1 should be(removedClass.size)
        "class11" should be(removedClass.head.attribute("className").mkString)
      }
    }
  }
}
