package laziness

import org.scalatest._

class StreamSpec extends FlatSpec with Matchers {
  "An empty stream" should "return an empty list" in {
    Empty.toList shouldBe List.empty
  }

  "A nonempty stream" should "becomes a nonempty list" in {
    Stream("a", "b", "c").toList shouldBe List("a", "b", "c")
  }

  "A nonempty stream" should "take the first n and turn into a stream" in {
    Stream("a", "b", "c", "d", "e").take(3).toList shouldBe Stream("a", "b", "c").toList
  }

  "A nonempty stream" should "drop the first n and turn the remaining into a stream" in {
    Stream("a", "b", "c", "d", "e").drop(3).toList shouldBe Stream("d", "e").toList
  }

  "A nonempty stream" should "take while true and turn into a stream" in {
    Stream("a", "b", "c", "d", "e").takeWhile(_ != "d").toList shouldBe Stream("a", "b", "c").toList
  }
}
