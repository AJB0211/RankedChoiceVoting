package com.ajb0211.RankedChoiceVoting

type Ballots[T] = List[List[T]]

object main extends App {

  def firstElems(ballots: Ballots[T]): List[T] = ballots.map(_.head)

  def rmEmpty(ballots: Ballots[T]): Ballots[T] = ballots.filter(_.nonEmpty)

  def rmCandidate(ballots: Ballots[T], candidate: T): Ballots[T] = ballots.map(_.filter(_!=candidate))

  def rmCandidates(ballots: Ballots[T], candidates: List[T]): Ballots[T] = {
    ballots
      .map(_.filterNot(
        candidates.contains(_)
      ))
  }

  def getMissing(ballots: Ballots[T], candidates: List[T]): List[T] = {

  }

}