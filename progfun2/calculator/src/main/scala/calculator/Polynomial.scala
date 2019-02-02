package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    new Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    new Signal(
        if (delta() < 0) Set()
        else {
            val deltaRoot = if (delta() < 0) 0 else Math.sqrt(delta())
            val root1 = (-b() + deltaRoot) / (2 * a())
            val root2 = (-b() - deltaRoot) / (2 * a())
            Set(root1, root2)
        }
    )
  }
}
