import scala.annotation.tailrec
import scala.collection.MapView
import scala.io.Source


object main extends App {
  type Ballots[T] = List[List[T]]

  def firstElems[T](ballots: Ballots[T]): List[T] = ballots.map(_.head)

  def rmEmpty[T](ballots: Ballots[T]): Ballots[T] = ballots.filter(_.nonEmpty)

  def rmCandidate[T](ballots: Ballots[T], candidate: T): Ballots[T] = ballots.map(_.filter(_!=candidate))

  def rmCandidates[T](ballots: Ballots[T], candidates: List[T]): Ballots[T] = {
    ballots
      .map(_.filterNot(
        candidates.contains(_)
      ))
  }

  def getMissing[T](ballots: Ballots[T], candidates: List[T]): List[T] = ballots.flatten.filterNot(candidates.contains(_))

  def rmMissing[T](ballots: Ballots[T], candidates: List[T]): Ballots[T] = rmCandidates(ballots, getMissing(ballots, candidates))

  def makeCountMap[T](xs : List[T]): MapView[T, Int] = xs.groupBy(identity).view.mapValues(_.size)

  def countVotes[T](ballots: Ballots[T]): MapView[T,Int] = makeCountMap(firstElems(ballots))

  def minCandidate[T](ballots: Ballots[T]): T = countVotes(ballots).minBy(_._2)._1

  def maxCandidate[T](ballots: Ballots[T]): T = countVotes(ballots).maxBy(_._2)._1

  def InstantRunoffVoting[T](ballots : Ballots[T], threshold : Double = 0.5): Option[T] = {
    @tailrec
    def inner(ballots: Ballots[T], winThresh : Double): Option[T] = {
      if (rmEmpty(ballots).isEmpty) {
        return None
      }

      val maxCandidate = countVotes(ballots)maxBy(_._2)
      if (maxCandidate._2 >= winThresh) {
        return Some(maxCandidate._1)
      }
      else {
        inner(
          rmEmpty(
            rmCandidate(
              rmMissing(ballots, firstElems(ballots)),
              minCandidate(ballots)
          )),
          winThresh
        )}}

    inner(ballots, ballots.size * threshold)
  }

  def readVotes(fileName: String): Ballots[String] = {
    val src = Source.fromFile(fileName)
    val out = src.getLines.map(_.split(", ").toList).toList
    src.close
    out
  }


  println(
    InstantRunoffVoting(
    readVotes(
      args(0)
    )) match {
      case Some(winner) => s"The winner is $winner"
      case _ => "The ballot is undecidable"
    }
  )



}
