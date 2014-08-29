package mandelbrot

case class AsciiRenderer(min: Int = 0, max: Int = 256) {
  
  val palette = Seq(" ", ".", "*", "+", "&", "8", "#", "@")
  val lenght = max - min
  require(max > min, s"max $max should be greater than min $min")

  def render(x: Int): String = {
    require(x >= min, s"value $x should be greater or equal than $min")
    require(x < max, s"value $x should be less than $max")
    palette(x * palette.size / lenght)
  }
}
