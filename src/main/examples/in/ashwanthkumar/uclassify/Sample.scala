package in.ashwanthkumar.uclassify

import dispatch.Defaults._

object Sample extends App {
  /*
    Assumes we have "application.conf" in classpath with
    read and write API Keys. See reference.conf for usage.
   */
  val client = UClassifyClient("__READ__API__KEY", "__WRITE__API__KEY")

  val textsToTrain = List(
    "negative" -> "i hate you",
    "negative" -> "he hates you",
    "negative" -> "she hates you",
    "negative" -> "it hates you",
    "positive" -> "i love you",
    "positive" -> "she likes me",
    "positive" -> "he likes me",
    "positive" -> "i like it"
  )

  val classifierName: String = "TestClassifier_1"
  client.createClassifier(classifierName)
  client.addClass("positive", classifierName)
  client.addClass("negative", classifierName)
  client.train(textsToTrain.filter(_._1.equals("positive")).map(_._2), "positive", classifierName)
  client.train(textsToTrain.filter(_._1.equals("negative")).map(_._2), "negative", classifierName)

  for {
    result <- client.classify(classifierName, List("she love me", "he hates me"))
  } yield result.foreach(println)

  // You can enable the following line to remove the test classifier automatically
  //  client.removeClassifier(classifierName)
}
