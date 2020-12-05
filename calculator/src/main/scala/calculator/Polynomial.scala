package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal (b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(
      if (delta() < 0) Set()
      else {
        val sol = (- b() - Math.sqrt(delta())) / (2 * a())
        val sol2 = (- b() + Math.sqrt(delta())) / (2 * a())
        Set(sol, sol2)
      }
    )
  }
}