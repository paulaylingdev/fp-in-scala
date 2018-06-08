package exercise7

import java.util.concurrent.{ExecutorService, Executors, Future}

import org.scalatest.{FlatSpec, Matchers}

class ParTest extends FlatSpec with Matchers {

  val executor: ExecutorService = Executors.newFixedThreadPool(20)

  "sum" should "correctly sum up a list of integers" in {
    val result: Future[Int] = Par.sum(IndexedSeq(5,4,2,8,10,23), default = 0)(_ + _)(executor)
    result.get shouldBe 52
  }

}