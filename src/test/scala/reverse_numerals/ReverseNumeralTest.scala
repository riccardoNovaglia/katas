package reverse_numerals

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class ReverseNumeralTest extends FlatSpec with Matchers with TableDrivenPropertyChecks {

  val examples = Table(
    ("roman", "arabic"),
    ("I", 1),
    ("II", 2),
    ("III", 3),
    ("IV", 4),
    ("V", 5),
    ("VI", 6),
    ("VII", 7),
    ("VIII", 8),
    ("IX", 9),
    ("X", 10),
    ("CIV", 104),
    ("CXLIV", 144)

  )

  forAll(examples) { (roman: String, arabic: Int) =>
    "Reverse roman numeral" should s"convert $roman to $arabic" in {
      RomanNumeral(roman) shouldBe arabic
    }
  }
}

object RomanNumeral {
  val symbolsTranslations: Map[Char, Int] = Map(
    ('I', 1),
    ('V', 5),
    ('X', 10),
    ('L', 50),
    ('C', 100)
  )

  val canSubtract = Map(
    ('I', List('V', 'X')),
    ('X', List('L', 'C'))
  )

  def apply(s: String): Int = {
    f(s)
  }

  def f(s: String): Int = {
    s.toList match {
      case x :: Nil => symbolsTranslations.getOrElse(x, 0)
      case x :: xs => checkSubtraction(x, xs.head) + f(xs.tail.mkString(""))
      case Nil => 0
    }
  }

  def checkSubtraction(x: Char, y: Char): Int = {
    canSubtract.get(x) match {
      case Some(value) if value.contains(y) => f(y.toString) - f(x.toString)
      case _ => f(x.toString) + f(y.toString)
    }
  }
}
