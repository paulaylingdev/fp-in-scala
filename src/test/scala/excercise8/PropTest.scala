package excercise8

import org.scalacheck.Gen._
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll
import org.scalatest.prop.Checkers._
import org.scalatest.{FlatSpec, Matchers}

class PropTest extends FlatSpec with Matchers {

    "sum" should "sum up a list of integers" in {
      val intList = listOf(choose(0, 100))
      val prop: Prop = forAll(intList)(ns => ns.reverse.sum == ns.sum)
      check(prop)
    }

    "max" should "find the maximum integer in a list" in {
      val max = 125
      val intList = listOf(choose(0, max)) suchThat(_.contains(max))
      val prop = forAll(intList)(ns => ns.max == max)
      check(prop)
    }
}
