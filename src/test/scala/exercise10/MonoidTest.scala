package exercise10

import exercise10.Monoids._
import exercise10.Laws._
import exercise8.{Gen, Passed}
import main.scala.exercise6.SimpleRNG
import org.scalatest.{FlatSpec, Matchers}

class MonoidTest extends FlatSpec with Matchers {

  "intAddition" should "add integers together" in {
    associative(intAddition)(5, 4, 3) shouldBe true
    identity(intAddition)(3) shouldBe true
  }

  "intMultiplication" should "multiply integers" in {
    associative(intMultiplication)(5, 3, 2) shouldBe true
    identity(intMultiplication)(2) shouldBe true
  }

  "booleanOr" should "perform logical OR on booleans" in {
    associative(booleanOr)(true, false, false) shouldBe true
    identity(booleanOr)(true) shouldBe true
  }

  "booleanAnd" should "perform logical AND on booleans" in {
    associative(booleanAnd)(true, false, false) shouldBe true
    identity(booleanAnd)(true) shouldBe true
  }

  "optionMonoid" should "be a monoid" in {
    associative(optionMonoid[Int])(Some(5), None, Some(4)) shouldBe true
    identity(optionMonoid[Int])(Some(3)) shouldBe true
  }

  "endoMonoid" should "be a monoid" in {
    def endoFunction(s : String): String = s
    def endoFunction2(s : String): String = s + "b"

    associative(endoMonoid[String])(endoFunction, endoFunction2, endoFunction)
    identity(endoMonoid[String])(endoFunction2)
  }

  "intAddition" should "satisfy monoidLaws" in {
    val simpleRNG = SimpleRNG(12345)
    monoidLaws(intAddition, Gen.choose(-100, 100)).run(1000, 1000, simpleRNG) shouldBe Passed
  }

}
