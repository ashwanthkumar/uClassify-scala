package in.ashwanthkumar.uclassify

import scala.xml.{Node, Elem}

case class ClassifiedClass(confidence: Double, className: String)

case class ClassifiedText(text: String, textCoverage: Double, classes: List[ClassifiedClass])

object ClassifiedText {
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
}

object ResponseTransformer {

  import ClassifiedText._

  def transformClassifyResult(texts: List[String])(response: Elem) = {
    for {classified <- response \\ "classify"} yield toClassifiedText(classified, texts)
  }
}
