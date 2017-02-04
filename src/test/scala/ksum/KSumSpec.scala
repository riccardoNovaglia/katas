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
    println("Fastest executions: (op name, execution time in ns)")

    val benchmark = sortedNamesTimes(
      Map(
        "Fors" -> KSum.fors,
        "Recursion with set" -> KSum.recsWithSet,
        "Recursion without set" -> KSum.fors,
        "Fold with set" -> KSum.fors,
        "Fold no set" -> KSum.fors
      ),
      ((1 to 1000).toList, 10000))

    println(benchmark)
  }

  private def sortedNamesTimes(ops: Map[String, (List[Int], Int) => Boolean], args: (List[Int], Int)): List[(String, Long)] = {
    val nameAndTime = ops.map(
      (op: (String, (List[Int], Int) => Boolean)) =>
        (
          op._1,
          execTimeFor(op._1)(op._2(args._1, args._2))
        )
    )

    nameAndTime.toList.sortWith((first, second) => first._2.compareTo(second._2) < 0)
  }

  private def execTimeFor[T](opName: String)(op: => T): Long = {
    val start = System.nanoTime()
    op
    System.nanoTime() - start
  }
}
