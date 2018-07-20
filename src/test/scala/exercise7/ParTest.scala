package exercise7

import java.util.concurrent.{ExecutorService, Executors, Future}

import org.scalatest.{FlatSpec, Matchers}

class ParTest extends FlatSpec with Matchers {

  val executor: ExecutorService = Executors.newFixedThreadPool(20)

  "sum" should "correctly sum up a list of integers" in {
    val result: Future[Int] = Par.sum(IndexedSeq(5, 4, 2, 8, 10, 23), default = 0)(_ + _)(executor)
    result.get shouldBe 52
  }

  "countParagraphs" should "take a list of paragraphs and return the total number of words" in {
    val result = Par.countParagraphs(List("Easy as one two three", "a b c"))(executor)
    result.get shouldBe 8
  }

  "choiceN" should "take a choice integer of N and run one of the choices provided" in {
    val result = Par.choiceN(Par.unit(1))(List(Par.unit("a"), Par.unit("b"), Par.unit("c")))(executor)
    result.get shouldBe "b"
  }

  "choice" should "take a boolean condition and execute one parallel or another" in {
    val result = Par.choice(Par.unit(true))(Par.unit("trueValue"), Par.unit("falseValue"))(executor)
    result.get shouldBe "trueValue"

    val result2 = Par.choice(Par.unit(false))(Par.unit("trueValue"), Par.unit("falseValue"))(executor)
    result2.get shouldBe "falseValue"
  }

  "choiceMap" should "take a key and map it to a choice" in {
    val result = Par.choiceMap(Par.unit("123"))(Map("123" -> Par.unit(123)))(executor)
    result.get shouldBe 123
  }

  "chooser" should "take a value and a function that maps the value to a result" in {
    val result = Par.chooser(Par.unit("123"))((s: String) => Par.unit(s.toInt))(executor)
    result.get shouldBe 123
  }

  "chooserChoiceN" should "take a choice integer of N and run one of the choices provided" in {
    val result = Par.chooserChoiceN(Par.unit(1))(List(Par.unit("a"), Par.unit("b"), Par.unit("c")))(executor)
    result.get shouldBe "b"
  }

  "chooserChoice" should "take a boolean condition and execute one parallel or another" in {
    val result = Par.chooserChoice(Par.unit(true))(Par.unit("trueValue"), Par.unit("falseValue"))(executor)
    result.get shouldBe "trueValue"
  }

  "join" should "execute a parallel computation within another" in {
    val result = Par.join(Par.unit(Par.unit(3)))(executor)
    result.get shouldBe 3
  }

  "flatMap" should "compute the initial value and use the reuslt in another calculation" in {
    val result = Par.flatMap(Par.unit("abc"))((s: String) => Par.unit(s.charAt(2)))(executor)
    result.get shouldBe 'c'
  }

  "joinFlatMap" should "be exactly the same as join but implemented using flatMap" in {
    val result = Par.joinFlatMap(Par.unit(Par.unit(3)))(executor)
    result.get shouldBe 3
  }

  "map2FlatMap" should "be exactly the same as map2 but implemented using flatMap" in {
    val result = Par.map2FlatMap(Par.unit(2), Par.unit(3))((a, b) => (a * b).toString)(executor)
    result.get shouldBe "6"
  }
}