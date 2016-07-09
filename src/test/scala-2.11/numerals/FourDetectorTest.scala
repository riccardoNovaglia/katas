package numerals

import org.scalatest.{FlatSpec, Matchers}

class FourDetectorTest extends FlatSpec with Matchers {

  val detector = new FourDetector
  "Four detector" should "detect four characters in a row in a string" in {
    detector.get4Repeated("aaaa") shouldBe Some("aaaa")
    detector.get4Repeated("") shouldBe None
    detector.get4Repeated("ac") shouldBe None
    detector.get4Repeated("aaa") shouldBe None
  }

  it should "detect anywhere in the string" in {
    detector.get4Repeated("abbbbc") shouldBe Some("bbbb")
    detector.get4Repeated("abbbbb") shouldBe Some("bbbb")
    detector.get4Repeated("abbbcdbcccc") shouldBe Some("cccc")
    detector.get4Repeated("abcdefgggg") shouldBe Some("gggg")
    detector.get4Repeated("abcdegf") shouldBe None
  }
}
