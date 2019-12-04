
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops

    var c_r = 0
    var c_g = 0
    var c_b = 0
    var c_a = 0
    var count: Int = 0
    var a = clamp(x - radius, 0, src.width - 1)
    val a_max = clamp(x + radius, 0, src.width - 1)
    val b_max = clamp(y + radius, 0, src.height - 1)
    val b_min = clamp(y - radius, 0, src.height - 1)
    while (a <= a_max){
      var b = b_min
      while (b <= b_max){
        c_r = c_r + red(src.apply(a, b))
        c_g = c_g + green(src.apply(a, b))
        c_b = c_b + blue(src.apply(a, b))
        c_a = c_a + alpha(src.apply(a, b))
        b = b + 1
        count = count + 1
      }
      a = a + 1
    }
    rgba(c_r / count,c_g / count,c_b / count, c_a / count)
  }

}

/**
var i = 0
while (i < 12){
  var j = 0
  while(j < 12){
    scr.update(i, j, rgba(i + 1, j + 2, i + 3, j + 4))
    println("done for (" + i + ", " + j + ")")
    j = j + 1
  }
  println("done for " + i)
  i = i + 1
}
  */
