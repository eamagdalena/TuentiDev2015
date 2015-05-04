package tuenti

import scala.collection.mutable.HashMap

/**
 * @author Eduardo
 */
case class XCorrelation(wave: List[Double], pattern: List[Double]) {
  import XCorrelation._ /* Importing companion object */

  val THRESCORR = 1e-30

  lazy val y = pattern

  /* 1. We precalculate this */
  lazy val yMean = y.filter(yi => yi >= 0 && yi <= 15).sum / y.size
  lazy val ySumCuadraticDiff = y.map { yi => Math.pow(yi - yMean, 2) }.sum
  lazy val sqrtySumCuadraticDiff = Math.sqrt(ySumCuadraticDiff)
  lazy val yMinusMean = y.map { _ - yMean }

  def crosscorr(x: List[Double]): List[Double] = {

    /* 2. Avoid unnecesary calculations */
    if (x.size > y.size) return List.empty

    val xMean = x.sum / x.size //filter(xi => xi >= 0 && xi <= 15)

    val xSumCuadraticDiff = x.map { xi => Math.pow(xi - xMean, 2) }.sum
    val denom = Math.sqrt(xSumCuadraticDiff) * sqrtySumCuadraticDiff

    if (denom < THRESCORR) return Nil

    val res = (for {
      delay <- 0 to (y.size - x.size)
      yMinusMeanIter = yMinusMean.iterator drop (delay)
      xIter = x.iterator
    } yield {
      xIter.map { xi => (xi - xMean) * yMinusMeanIter.next }.sum / denom
    }).toList

    res

  }

  def findScore: Double = {

    val minSubvectorLength = 2
    val roundsForBeingChampion = 100

    var maxScore: Double = 0
    var roundsAsMax: Double = 0

    /* Happy Idea : Normalised Cross Correlation follows a semi gaussian distribution: Lets find the max! */

    for (size <- (minSubvectorLength to pattern.size).reverse) {

      val score = findScoreForSize(size)

      if (score > maxScore) {
        maxScore = score
        roundsAsMax = 0
      } else {
        roundsAsMax += 1
      }

      if (roundsAsMax == roundsForBeingChampion) {
        return maxScore
      }

    }

    maxScore
  }

  def findScoreForSize(size: Int): Double = {

    val maxCorrelationForThisSize =
      (for {
        subvectorStart <- 0 to wave.size - size
        xcorrelation = crosscorr(wave.drop(subvectorStart).take(size))
        if (!xcorrelation.isEmpty)
      } yield xcorrelation.max).max * size

    maxCorrelationForThisSize

  }

  object XCorrelation {
    def min(x: Int, y: Int) = if (x < y) x else y
    def max(x: Double, y: Double) = if (x > y) x else y
  }

}
