package mandelbrot

import org.specs2.mutable._

class AsciiRendererSpec extends Specification {

  def check(pair: (Int, String))(implicit asciiRenderer: AsciiRenderer) = 
    asciiRenderer.render(pair._1) must beEqualTo(pair._2)
	
  "Ascii Art" should {
    "render characters in" in {
      "a range from 0 to 7" in {
        implicit val a = AsciiRenderer(0, 8)
        Seq(
          0 -> " ", 1 -> ".", 2 -> "*", 3 -> "+", 
          4 -> "&", 5 -> "8", 6 -> "#", 7 -> "@"
        ).map(check)
      }
      "a range from 0 to 255" in {
        implicit val a = AsciiRenderer(0, 255)
        Seq(
          0 -> " ", 32 -> ".", 64 -> "*", 96 -> "+", 
          128 -> "&", 160 -> "8", 192 -> "#", 224 -> "@"
        ).map(check)
      }
    }
  }
}