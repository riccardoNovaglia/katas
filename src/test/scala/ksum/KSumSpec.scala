package ksum

import org.scalatest.{FlatSpec, Matchers}

class KSumSpec extends FlatSpec with Matchers {

  "K Sum" should "return true if two values in a list add up to a given value - for loops" in {
    KSum.fors(List(1, 2, 3, 4, 5), 8) should be(true)
    KSum.fors(List(1, 2, 3, 4, 5), 15) should be(false)
  }

  it should "do it with recursion" in {
    KSum.recs(List(1, 2, 3, 4, 5), 8) should be(true)
    KSum.recs(List(1, 2, 3, 4, 5), 15) should be(false)
  }

  it should "do it with folding" in {
    KSum.folding(List(1, 2, 3, 4, 5), 8) should be(true)
    KSum.folding(List(1, 2, 3, 4, 5), 15) should be(false)
  }

  it should "do it with folding and a set to be faster" in {
    KSum.foldingWithSet(List(1, 2, 3, 4, 5), 8) should be(true)
    KSum.foldingWithSet(List(1, 2, 3, 4, 5), 15) should be(false)
  }

  it should "do it with recursion and a set to be faster" in {
    KSum.recsWithSet(List(1, 2, 3, 4, 5), 8) should be(true)
    KSum.recsWithSet(List(1, 2, 3, 4, 5), 15) should be(false)
  }

  it should "benchmark" in {
    val namesAndTimes = sortedNamesTimesGen[List[Int], Int, Boolean](
      (1 to 1000).toList, 1000) {
      Map(
        "Fors" -> KSum.fors,
        "Recursion with set" -> KSum.recsWithSet,
        "Recursion without set" -> KSum.recs,
        "Fold with set" -> KSum.foldingWithSet,
        "Fold no set" -> KSum.folding
      )
    }

    println(namesAndTimes.foreach((nameAndTime: (String, Long)) => println(nameAndTime._1, nameAndTime._2)))
  }

  private def sortedNamesTimesGen[ARG1, ARG2, RETURN](arg1: ARG1, arg2: ARG2)(ops: Map[String, (ARG1, ARG2) => RETURN]): List[(String, Long)] = {
    val nameAndTime = ops.map {
      case (functionName, function) => (functionName, execTime(function(arg1, arg2)))
    }

    nameAndTime.toList.sortWith((first, second) => first._2.compareTo(second._2) < 0)
  }

  private def execTime[T](op: => T): Long = {
    val start = System.nanoTime()
    op
    System.nanoTime() - start
  }
}
