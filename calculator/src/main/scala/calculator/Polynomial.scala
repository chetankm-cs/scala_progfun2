package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b()*b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    Signal{
      delta() match {
        case x if x < 0 => Set.empty
        case x if x == 0 => Set(-b()/ 2 * a())
        case deltaValue =>
          val deltaSqrt = Math.sqrt(deltaValue)
          Set((-b() + deltaSqrt) / (4 * a() * c()), (-b() - deltaSqrt) / 4 * a() * c())
      }
    }
  }
}
