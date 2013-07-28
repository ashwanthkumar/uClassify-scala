package in.ashwanthkumar.uclassify

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import dispatch.Defaults._
import scala.util.{Failure, Success}
import scala.concurrent.Await
import scala.concurrent.duration.DurationDouble

class UClassifyClientTest extends FunSuite with ShouldMatchers {
  val client = new UClassifyClient()

  test("should classify") {
    val testTexts = List("I hate you", "I love you")
    val response = client.classify("Sentiment", testTexts, Some("uClassify"))
    response onComplete {
      case Success(resp) => resp.foreach(println)
      case Failure(error) => println("Error message => " + error.getMessage)
    }

    Await.result(response, 10 seconds)
    //    print(response)
    //    println(resp)
  }
}
