package scalashop

import org.scalameter._
import common._
import java.util.concurrent._
import scala.collection.parallel.ForkJoinTasks

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 1
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 10
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    var x = from
    while (x < end){
      var y = 0
      while (y < src.height){
        dst.update(x, y, boxBlurKernel(src, x, y, radius))
        y = y + 1
      }
      x = x + 1
    }
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    val w = src.width
    val m = math.max(w / numTasks, 1)
    val lista1 = ((0 to w) by m).toList
    val lista2 = lista1.init ::: List(w)
    val lista = lista2.zip(lista2.tail)
    def listOfTask(list: List[(Int, Int)]): List[ForkJoinTask[Unit]] = list match {
      case Nil => List()
      case (n, m) :: ls => task{ blur(src, dst, n, m, radius) } :: listOfTask(ls)
    }
    val theTasks = listOfTask(lista)
    theTasks map (x => x.join())
  }

}
