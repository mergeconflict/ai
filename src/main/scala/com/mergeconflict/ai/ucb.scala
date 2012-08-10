package com.mergeconflict.ai

/**
 * Arm typeclass: any type A can be used as an arm of a multi-armed bandit if
 * playing the arm can return a reward (normalized between 0 and 1).
 */
trait Arm[A] {
  def play(arm: A): Double
}

/**
 * A multi-armed bandit
 */
class Bandit[A : Arm] private (private var stats: List[Stats[A]]) {

  private var plays: Int = 0

  def play: Unit = {
    // we always keep stats sorted by ucb, highest to lowest. extract
    // the head and play it to update the stats
    val head :: tail = stats
    head.play
    plays += 1

    // re-insert the former head into the list of stats, preserving its order
    val (lhs, rhs) = tail span { head.ucb(plays) < _.ucb(plays) }
    stats = lhs ::: head :: rhs
  }

  def best: A = stats.max(Stats.byRewardAverage).arm
}

object Bandit {
  def apply[A : Arm](arms: Traversable[A]) =
    new Bandit(arms.map(new Stats[A](_))(collection.breakOut): List[Stats[A]])
}

private class Stats[A : Arm](val arm: A) {

  private var plays: Int = 0
  private var rewardSum: Double = 0
  private var rewardSquaredSum: Double = 0
  private var rewardAverage: Double = 0
  private var rewardVariance: Double = 0

  def play: Unit = {
    val reward = implicitly[Arm[A]].play(arm)
    plays += 1
    rewardSum += reward
    rewardSquaredSum += math.pow(reward, 2)
    rewardAverage = rewardSum / plays
    rewardVariance = rewardSquaredSum / plays - math.pow(rewardAverage, 2)
  }

  def ucb(banditPlays: Int): Double = {
    val bias = math.log(banditPlays) / plays
    rewardAverage + math.sqrt(bias * math.min(0.25, rewardVariance + math.sqrt(bias * 2)))
  }

}

private object Stats {
  val byRewardAverage: Ordering[Stats[_]] = Ordering by { _.rewardAverage }
}