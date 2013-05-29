package graph.benchmark

trait BMark extends com.google.caliper.SimpleBenchmark {

  def run[A](n: Int)(f: ⇒ A) {
    var c = 0
    while (c < n) {
      val a = f
      c += 1
    }
  }
}

// vim: set ts=2 sw=2 et:
