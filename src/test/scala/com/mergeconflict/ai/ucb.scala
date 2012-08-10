package com.mergeconflict.ai

import org.scalacheck.{ Arbitrary, Gen }
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

case class Bernoulli(val distribution: Double) {
  def next: Boolean = util.Random.nextDouble <= distribution
}

object Bernoulli {

  implicit val bernoulliArm: Arm[Bernoulli] = new Arm[Bernoulli] {
    def play(arm: Bernoulli): Double = if (arm.next) 1 else 0
  }

}

class StatsSpec extends Specification with ScalaCheck {

  implicit val tenArbitraryBernoullis: Arbitrary[List[Bernoulli]] =
    Arbitrary(Gen.listOfN(10, Gen.choose[Double](0, 1) map { Bernoulli(_) }))

  "UCB" should {
    "always find a decent bandit arm" in check { bernoullis: List[Bernoulli] =>
      val sorted = bernoullis sortBy { _.distribution }
      val threshold = (sorted.last.distribution * 9 + sorted.head.distribution) / 10

      val bandit = Bandit(bernoullis)
      for { _ <- 0 until 10000 } bandit.play
      bandit.best.distribution must beGreaterThanOrEqualTo(threshold)
    }
  }

}