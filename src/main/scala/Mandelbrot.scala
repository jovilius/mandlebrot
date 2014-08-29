package mandelbrot

import scala.language.implicitConversions
import scala.annotation.tailrec
import Math._
import Complex._
import Orbit._

case class Orbit(c: Complex, x: Complex = 0, maxPoints: Int = MaxPoints, maxRadius: Double = 3) {
  private def magnitude(point: Complex): Double = !(point - c)
  private def points(z: Complex = quadraticPolynomial(c, x)): Stream[Complex] = z #:: points(quadraticPolynomial(c, z))
  def points: Stream[Complex] = points().take(maxPoints).takeWhile(magnitude(_) < maxRadius)
  def lenght: Int = points.size
  def isBounded: Boolean = lenght == maxPoints
}

object Orbit {
  val MaxPoints = 100
  def quadraticPolynomial(c: Complex, x: Complex) = x * x + c
}

object Mandelbrot {
  def main(args: Array[String]) {
    val asciiRenderer = AsciiRenderer(0, MaxPoints)
    val set = plane(z(-2, -1.1), z(0.5, 1.1), 80).map(_.map(c => Orbit(c).lenght - 1))
    val ascii = set.map(_.map(asciiRenderer.render).mkString).mkString("\n")
    println(ascii)    
  }
}
