package excercise8

import exercise8.Prop.TestCases
import exercise8.{Falsified, Gen, Passed, Prop}
import main.scala.exercise6.{RNG, SimpleRNG}
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

  "Gen.union" should "combine two generators of the same type into one by pulling values from each generator" in {
    val gen1 = Gen.choose(0, 100)
    val gen2 = Gen.choose(250,500)
    val unionGen = Gen.union(gen1, gen2)
    val result = generate(unionGen)
    result should be >= 0
    result should be < 500
  }

  "Gen.weighted" should "combine two generators of the same type into one by pulling values from each based on a weight" in {
    val gen1 = Gen.choose(0, 100)
    val gen2 = Gen.choose(250,500)
    val unionGen = Gen.weighted((gen1, 0.2), (gen2, 0.8))
    val result = generate(unionGen)
    result should be >= 250
    result should be < 500
  }

  "Prop.&&" should "combine two Prop objects into one using logical AND" in {
    val prop1 = Prop((_, _) => Passed)
    val prop2 = Prop((_, _) => Falsified("something went wrong!", 0))

    val combined = prop1 && prop2
    combined.run(1, simpleRNG) shouldBe Falsified("something went wrong!", 0)
  }

  "Prop.||" should "combine two Prop objects into one using logical OR" in {
    val prop1 = Prop((_, _) => Passed)
    val prop2 = Prop((_, _) => Falsified("something went wrong!", 0))

    val combined = prop1 || prop2
    combined.run(1, simpleRNG) shouldBe Passed
  }

  "Prop.tag" should "prefix a message to a failing properties output message" in {
    val prefix = "Boom"
    val prop1 = Prop((_, _) => Falsified("Error", 0))

    prop1.tag(prefix).run(1, simpleRNG) shouldBe Falsified(s"$prefix \n Error", 0)
  }

  private def generate[A](gen: Gen[A]): A = {
    gen.sample.run(simpleRNG)._1
  }
}
