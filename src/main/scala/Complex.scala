package mandelbrot

import scala.language.implicitConversions
import Math._
import Complex._

case class Complex(r: Double, i: Double) {
  def +(c: Complex) = z(r + c.r, i + c.i)
  def -(c: Complex) = z(r - c.r, i - c.i)
  def *(c: Complex) = z(r * c.r - i * c.i, r * c.i + i * c.r)
  lazy val unary_! = sqrt(pow(r, 2) + pow(i, 2)) 
  override def toString = r + (if (i < 0) s" - ${-i}i" else s" + ${i}i")
}

object Complex {
  def z(r: Double, i: Double) = Complex(r, i)
  def plane(bottomLeft: Complex, topRight: Complex, steps: Int): Array[Array[Complex]] = {
    val size = topRight - bottomLeft
    def r(n: Int) = bottomLeft.r + n * size.r / steps
    def i(n: Int) = bottomLeft.i + n * size.i / steps
    Array.tabulate(steps, steps)((y, x) => z(r(x), i(y)))
  }
  implicit def fromDouble(r: Double) = z(r, 0)
}
