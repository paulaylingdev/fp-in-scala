package exercise10

import exercise10.Monoids._
import exercise10.Laws._
import exercise7.Par.Par
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

  "concatenate" should "concatenate a list of strings" in {
    concatenate(List("A", "B", "C"), stringMonoid) shouldBe "ABC"
  }

  "foldMap" should "map elements in a list to a monoid and then concatenate them" in {
    foldMap(List(1, 2, 3), stringMonoid)(_.toString) shouldBe "123"
  }

  "foldRight" should "fold a list of elements into a single element" in {
    foldRight(List(1,2,3), "")((a, b) => a.toString + b) shouldBe List(1,2,3).foldRight("")((a, b) => a.toString + b)
  }

  "foldMapV" should "fold a list of elements into a single element" in {
    foldMapV(IndexedSeq(1,2,3,4,5,6,7,8), stringMonoid)(_.toString) shouldBe "12345678"
    foldMapV(IndexedSeq(), stringMonoid)(_.toString) shouldBe ""
  }

  "par" should "create a parallel monoid" in {
    par(stringMonoid) shouldBe a[Monoid[Par[String]]]
  }

  "parFoldMap" should "fold a list of elements into a single element in parallel" in {
    parFoldMap(IndexedSeq(1,2,3,4,5,6,7,8), stringMonoid)(_.toString) shouldBe a[Par[String]]
  }

}
