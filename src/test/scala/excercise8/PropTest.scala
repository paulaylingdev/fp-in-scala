package excercise8

import exercise8.Gen
import main.scala.exercise6.SimpleRNG
import main.scala.exercise6.State
import org.scalatest.{FlatSpec, Matchers}

class PropTest extends FlatSpec with Matchers {

  val simpleRNG = SimpleRNG(12345)

  "Gen.choose" should "generate integers in range start to stopExclusive" in {
    val gen = Gen.choose(0, 5)
    val result = gen.sample.run(simpleRNG)._1
    result should be < 5
    result should be >= 0
  }

  "Gen.unit" should "create a Gen object of a given value" in {
    val gen = Gen.unit("abc")
    val result = gen.sample.run(simpleRNG)._1
    result shouldBe "abc"
  }

}
