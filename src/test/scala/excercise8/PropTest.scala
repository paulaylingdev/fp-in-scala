package excercise8

import exercise8.Gen
import main.scala.exercise6.SimpleRNG
import org.scalatest.{FlatSpec, Matchers}

class PropTest extends FlatSpec with Matchers {

  val simpleRNG = SimpleRNG(12345)

  "Gen.choose" should "generate integers in range start to stopExclusive" in {
    val gen = Gen.choose(0, 5)
    val result = generate(gen)
    result should be < 5
    result should be >= 0
  }

  "Gen.unit" should "create a Gen object of a given value" in {
    val gen = Gen.unit("abc")
    generate(gen) shouldBe "abc"
  }

  "Gen.boolean" should "generate a boolean" in {
    val gen = Gen.boolean
    generate(gen) shouldBe false //This specific seed generates false
  }

  "Gen.listOfN" should "generate a list of integers of size n" in {
    val gen = Gen.listOfN(5, Gen.choose(0, 5))
    val result = generate(gen)
    result shouldBe a [List[Int]]
    result should have size 5
  }

  "Gen.flatMap" should "generate a value then use that to determine what generator to use next" in {
    val gen: Gen[Boolean] = Gen.choose(0, 2).flatMap((number: Int) => Gen.unit(number % 2 == 0))
    generate(gen) shouldBe false
  }

  "Gen (case class).listOfN" should "generate a list of integers of size n" in {
    val gen = Gen.choose(0, 5).listOfN(Gen.unit(5))
    val result = generate(gen)
    result shouldBe a [List[Int]]
    result should have size 5
  }

  it should "generate a list of booleans of a random size" in {
    val gen = Gen.boolean.listOfN(Gen.choose(10,15))
    val result = generate(gen)
    result shouldBe a [List[Boolean]]
    result.size should be >= 10
    result.size should be < 15
  }

  private def generate[A](gen: Gen[A]): A = {
    gen.sample.run(simpleRNG)._1
  }
}
