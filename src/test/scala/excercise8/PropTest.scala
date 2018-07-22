package excercise8

import exercise8.Gen
import org.scalatest.{FlatSpec, Matchers}

class PropTest extends FlatSpec with Matchers {

  "Gen.choose" should "generate integers in range start to stopExclusive" in {
    val result = Gen.choose(0, 5)
    result.isInstanceOf[Gen[Int]] shouldBe true
  }

}
