package in.ashwanthkumar.uclassify

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class RequestBuilderTest extends FunSpec with ShouldMatchers {

  val testAPI = APIInfo("readKey", "writeKey")
  val requestSystem = RequestBuilder(testAPI)

  describe("TextBase64") {
    it("should return xml on toXML") {
      TextBase64("text1", "someText").toXml should be(<textBase64 id='text1'>c29tZVRleHQ=</textBase64>)
    }
  }

  describe("Request System") {
    describe("should construct xml requests for readCalls") {
      it("classify") {
        val classifyRequest = requestSystem.classify("testClassifier", List("some text1", "some text2"))

        val classifierNames = (classifyRequest \\ "classify").map(_ \ "@classifierName")
        2 should be(classifierNames.size)
        "testClassifier" should be(classifierNames.head.mkString)

        val readApiKey = (classifyRequest \\ "readCalls").map(_ \ "@readApiKey")
        readApiKey.head.mkString should be("readKey")

        val textsBase64 = (classifyRequest \\ "textBase64").map(_.text)
        2 should be(textsBase64.size)
        assert(textsBase64.contains(Utils.base64Encode("some text1")))
        assert(textsBase64.contains(Utils.base64Encode("some text2")))
        val textsBase64Ids = (classifyRequest \\ "textBase64").map(_ \ "@id").map(_.mkString)
        2 should be(textsBase64Ids.size)
        assert(textsBase64Ids.contains("text1"))
        assert(textsBase64Ids.contains("text2"))
      }

      it("classifyKeywords") {
        val classifyKeywordsRequest = requestSystem.classifyKeywords("testClassifier", List("some text1", "some text2"))

        val classifierNames = (classifyKeywordsRequest \\ "classifyKeywords").map(_ \ "@classifierName")
        2 should be(classifierNames.size)
        "testClassifier" should be(classifierNames.head.mkString)

        val textsBase64 = (classifyKeywordsRequest \\ "textBase64").map(_.text)
        2 should be(textsBase64.size)
        assert(textsBase64.contains(Utils.base64Encode("some text1")))
        assert(textsBase64.contains(Utils.base64Encode("some text2")))
      }

      it("getInformation") {
        val getInformationRequest = requestSystem.getInformation("testClassifier")

        val classifierNames = (getInformationRequest \\ "getInformation").map(_ \ "@classifierName")
        1 should be(classifierNames.size)
        "testClassifier" should be(classifierNames.head.mkString)

        val textsBase64 = (getInformationRequest \\ "textBase64").map(_.text)
        0 should be(textsBase64.size)
      }
    }
    describe("should construct xml requests for writeCalls") {
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

        1 should be((removeClassifierRequest \\ "writeCalls").size)
        val createCalls = removeClassifierRequest \\ "remove"
        1 should be(createCalls.size)
      }

      it("addClass") {
        val addClassRequest = requestSystem.addClass("class11", "testClassifier")

        1 should be((addClassRequest \\ "writeCalls").size)
        val addedClass = addClassRequest \\ "addClass"
        1 should be(addedClass.size)
        "class11" should be(addedClass.head.attribute("className").mkString)
      }

      it("removeClass") {
        val removeClassRequest = requestSystem.removeClass("class11", "testClassifier")

        1 should be((removeClassRequest \\ "writeCalls").size)
        val removedClass = removeClassRequest \\ "removeClass"
        1 should be(removedClass.size)
        "class11" should be(removedClass.head.attribute("className").mkString)
      }

      it("train") {
        val sampleTexts = List("sample text1", "sample text2")
        val trainingRequest = requestSystem.train(sampleTexts, "class11", "testClassifier")

        1 should be((trainingRequest \\ "writeCalls").size)
        2 should be((trainingRequest \\ "train").size)
        "class11" should be((trainingRequest \\ "train").map(_ \ "@className").head.mkString)
      }

      it("untrain") {
        val sampleTexts = List("sample text1", "sample text2")
        val trainingRequest = requestSystem.untrain(sampleTexts, "class11", "testClassifier")

        1 should be((trainingRequest \\ "writeCalls").size)
        2 should be((trainingRequest \\ "untrain").size)
        "class11" should be((trainingRequest \\ "untrain").map(_ \ "@className").head.mkString)

        val untrainTexts = (trainingRequest \\ "textBase64").map(_.text)
        assert(untrainTexts.contains(Utils.base64Encode("sample text1")))
        assert(untrainTexts.contains(Utils.base64Encode("sample text2")))
      }
    }
  }
}
