package excercise8

import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.Checkers._

class PropTest extends FlatSpec with Matchers {

    "sum" should "sum up a list of integers" in {
      val intList = Gen.listOf(Gen.choose(0, 100))
      val prop: Prop = forAll(intList)(ns => ns.reverse.sum == ns.sum)
      check(prop)
    }
}
