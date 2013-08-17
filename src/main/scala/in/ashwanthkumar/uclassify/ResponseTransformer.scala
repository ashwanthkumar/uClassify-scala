package in.ashwanthkumar.uclassify

import scala.xml.{Node, Elem}

case class ClassifiedClass(confidence: Double, className: String)
case class ClassifiedText(text: String, textCoverage: Double, classes: List[ClassifiedClass])
case class ClassInformation(name: String, uniqueFeatures: Int, totalCount: Int)

object ResponseTransformer {

  def transformClassifyResult(texts: List[String])(response: Elem) = {
    def toClassifiedText(xmlElem: Node, texts: List[String]) = {
      val classes =
        for {classificationClass <- xmlElem \\ "class"} yield ClassifiedClass(classificationClass
                                                                              .attribute("p")
                                                                              .mkString
                                                                              .toDouble,
          classificationClass.attribute("className").mkString)

      val classifyId = (xmlElem \\ "@id").mkString.replaceAll("classify", "").toInt
      ClassifiedText(texts(classifyId), (xmlElem \\ "@textCoverage").mkString.toDouble, classes.toList)
    }

    verifyResponse(response)
    for {classified <- response \\ "classify"} yield toClassifiedText(classified, texts)
  }

  def transformGetInformationResult(response: Elem) = {
    def toClassInfo(xmlElem: Node) = {
      ClassInformation(
        name = (xmlElem \\ "@className").text,
        uniqueFeatures = (xmlElem \\ "uniqueFeatures").text.toInt,
        totalCount = (xmlElem \\ "totalCount").text.toInt
      )
    }
    verifyResponse(response)
    for {classInfo <- response \\ "classInformation"} yield toClassInfo(classInfo)
  }


  def verifyResponse(response: Elem) {
    val statusNode = (response \\ "status")(0)
    if (!(statusNode \\ "@success").text.toBoolean) sys.error(statusNode.text)
  }
}
