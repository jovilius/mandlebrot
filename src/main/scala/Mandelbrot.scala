package mandelbrot

import scala.language.implicitConversions
import scala.annotation.tailrec
import Math._
import Complex._
import Orbit._

case class Orbit(c: Complex, x: Complex = 0, n: Int = 20) {
  def points(z: Complex = quadraticPolynomial(c, x)): Stream[Complex] = z #:: points(quadraticPolynomial(c, z))
  def isBounded(radius: Double = 2): Boolean = points().take(n).forall(!_ < radius)
}

object Orbit {
  def quadraticPolynomial(c: Complex, x: Complex) = x * x + c
}

object Mandelbrot {
  def main(args: Array[String]) {
    val set = plane(z(-2, -1), z(0.5, 1), 80).map(_.map(c => Orbit(c).isBounded()))
    val ascii = set.map(_.map(if (_) '*' else ' ').mkString).mkString("\n")
    println(ascii)    
  }
}
