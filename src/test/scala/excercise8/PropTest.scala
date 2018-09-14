package excercise8

import exercise7.Par
import exercise7.Par.Par
import exercise8._
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
    result shouldBe a[List[Int]]
    result should have size 5
  }

  "Gen.flatMap" should "generate a value then use that to determine what generator to use next" in {
    val gen: Gen[Boolean] = Gen.choose(0, 2).flatMap((number: Int) => Gen.unit(number % 2 == 0))
    generate(gen) shouldBe false
  }

  "Gen (case class).listOfN" should "generate a list of integers of size n" in {
    val gen = Gen.choose(0, 5).listOfN(Gen.unit(5))
    val result = generate(gen)
    result shouldBe a[List[Int]]
    result should have size 5
  }

  it should "generate a list of booleans of a random size" in {
    val gen = Gen.boolean.listOfN(Gen.choose(10, 15))
    val result = generate(gen)
    result shouldBe a[List[Boolean]]
    result.size should be >= 10
    result.size should be < 15
  }

  "Gen.union" should "combine two generators of the same type into one by pulling values from each generator" in {
    val gen1 = Gen.choose(0, 100)
    val gen2 = Gen.choose(250, 500)
    val unionGen = Gen.union(gen1, gen2)
    val result = generate(unionGen)
    result should be >= 0
    result should be < 500
  }

  "Gen.weighted" should "combine two generators of the same type into one by pulling values from each based on a weight" in {
    val gen1 = Gen.choose(0, 100)
    val gen2 = Gen.choose(250, 500)
    val unionGen = Gen.weighted((gen1, 0.2), (gen2, 0.8))
    val result = generate(unionGen)
    result should be >= 250
    result should be < 500
  }

  "Prop.&&" should "combine two Prop objects into one using logical AND" in {
    val prop1 = Prop((_, _, _) => Passed)
    val prop2 = Prop((_, _, _) => Falsified("something went wrong!", 0))

    val combined = prop1 && prop2
    combined.run(1, 1, simpleRNG) shouldBe Falsified("something went wrong!", 0)
  }

  "Prop.||" should "combine two Prop objects into one using logical OR" in {
    val prop1 = Prop((_, _, _) => Passed)
    val prop2 = Prop((_, _, _) => Falsified("something went wrong!", 0))

    val combined = prop1 || prop2
    combined.run(1, 1, simpleRNG) shouldBe Passed
  }

  "Prop.tag" should "prefix a message to a failing properties output message" in {
    val prefix = "Boom"
    val prop1 = Prop((_, _, _) => Falsified("Error", 0))

    prop1.tag(prefix).run(1, 1, simpleRNG) shouldBe Falsified(s"$prefix \n Error", 0)
  }

  "Gen.unsized" should "convert a Gen to SGen" in {
    val gen = Gen.boolean
    val sgen = gen.unsized
    sgen shouldBe a[SGen[Boolean]]
    sgen.forSize(123) shouldBe gen
    sgen.forSize(12345) shouldBe gen
  }

  "Gen.listOf" should "generate lists of the requested size" in {
    val gen = Gen.boolean
    val sgen = Gen.listOf(gen)
    sgen shouldBe a[SGen[List[Boolean]]]
    generate(sgen(2)) should have size 2
    generate(sgen(5)) should have size 5
  }

  "List.max" should "give an item that is greater than or equal to every other element" in {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    maxProp.run(100, 100, simpleRNG) shouldBe Passed

    Prop.run(maxProp)
  }

  "Gen.listOf1" should "generate a non empty list" in {
    val gen = Gen.boolean
    val sgen = Gen.listOf1(gen)
    generate(sgen(0)) should have size 1
    generate(sgen(-1)) should have size 1
    generate(sgen(2)) should have size 2
  }

  "List.sorted" should "sort a list of integers correctly" in {
    val smallInt = Gen.choose(-10, 10)
    val sortedProp = Prop.forAll(Gen.listOf(smallInt)) { input =>
      def isSorted[T](s: Seq[T])(implicit ord: Ordering[T]): Boolean = s match {
        case Seq() => true
        case Seq(_) => true
        case _ => s.sliding(2).forall { case List(x, y) => ord.lteq(x, y) }
      }

      val sortedList = input.sorted
      isSorted(sortedList)
    }
    sortedProp.run(100, 100, simpleRNG) shouldBe Passed
  }

  "Prop.check" should "prove or falsify a simple test" in {
    Prop.check(4 == 5).run(100, 100, simpleRNG) shouldBe Falsified("()", 0)
    Prop.check(5 == 5).run(100, 100, simpleRNG) shouldBe Proved
  }

  it should "prove that two Pars can be equal" in {
    val pint = Gen.choose(0, 10) map (Par.unit(_))
    Prop.forAllPar(pint)(n => Par.equal(Par.map(n)(y => y), n)).run(100, 100, simpleRNG) shouldBe Passed
  }

  it should "prove that two Par[Ints] can be equal" in {
    val pint: Gen[Par[Int]] = Gen.choose(0, 10).listOfN(Gen.choose(0, 20)).map(l =>
      l.foldLeft(Par.unit(0))((p, i) =>
        Par.fork {
          Par.map2(p, Par.unit(i))(_ + _)
        })
    )
    //    Prop.forAllPar(pint)(n => Par.equal(Par.map(n)(y => y), n)).run(100, 100, simpleRNG) shouldBe Passed
  }

  private def generate[A](gen: Gen[A]): A = {
    gen.sample.run(simpleRNG)._1
  }
}
