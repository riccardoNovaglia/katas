package numerals

import org.scalatest.{FunSuite, Matchers}

class FindRepeatedCharTest extends FunSuite with Matchers {

  test("should return the the string found between the two repeated chars") {
    findRepeated('f') in "abcdefgf" shouldBe Some("fgf")
  }

  test("should return none if no repeated chars are found") {
    findRepeated('a') in "abcdefg" shouldBe None
  }

  test("should return the whole string between the first and last repeated chars") {
    findRepeated('a') in "qwertyabcabca" shouldBe Some("abcabca")
  }
}
