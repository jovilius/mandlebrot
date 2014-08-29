package mandelbrot

import org.specs2.mutable._
import Complex._

class ComplexSpec extends Specification {

  "Complex number" should {
    "add" in {
      z(1, 3) + z(2, 5) must beEqualTo(z(3, 8))
    }
    "subtract" in {
      z(1, 3) - z(2, 5) must beEqualTo(z(-1, -2))
    }
    "multiply" in {
      z(1, 3) * z(2, 5) must beEqualTo(z(-13, 11))
    }
    "compute absolute value" in {
      !z(1, 3) must beEqualTo(3.1622776601683795)
    }
    "show" in {
      z(1, +2).toString must beEqualTo("1.0 + 2.0i")
      z(1, -2).toString must beEqualTo("1.0 - 2.0i")  
      z(1, 0).toString must beEqualTo("1.0 + 0.0i")
    }
    "be created from a Double" in {
      Complex.fromDouble(2.0) must beEqualTo(z(2, 0))
    }
  }

}
