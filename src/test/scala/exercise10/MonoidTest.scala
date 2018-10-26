package exercise10

import exercise10.Monoids._
import exercise10.Laws._
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

}
