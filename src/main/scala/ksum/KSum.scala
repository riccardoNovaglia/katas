package ksum

object KSum {
  def fors(ints: List[Int], target: Int): Boolean = {
    for (i <- ints.indices) {
      for (j <- ints.indices) {
        if (i != j && ints(i) + ints(j) == target) {
          return true
        }
      }
    }

    false
  }

  def recs(ints: List[Int], target: Int): Boolean = {
    ints match {
      case x :: xs => if (xs.contains(target - x)) true else recs(xs, target)
      case Nil => false
    }
  }

  def folding(ints: List[Int], target: Int): Boolean = {
    ints.foldLeft(false, List[Int]())((acc: (Boolean, List[Int]), value: Int) => {
      (acc._2.contains(target - value), acc._2 :+ value)
    })._1
  }

  def foldingWithSet(ints: List[Int], target: Int): Boolean = {
    ints.foldLeft(false, Set[Int]())((acc: (Boolean, Set[Int]), value: Int) => {
      (acc._2.contains(target - value), acc._2 + value)
    })._1
  }

  private def recsWithSet(ints: List[Int], target: Int, visited: Set[Int]): Boolean = {
    ints match {
      case x :: xs => if (visited.contains(target - x)) true else recsWithSet(xs, target, visited + x)
      case Nil => false
    }
  }

  def recsWithSet(ints: List[Int], target: Int): Boolean = {
    recsWithSet(ints, target, Set())
  }
}

