package reductions

import org.scalameter._

object LineOfSightRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> false
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }
}

sealed abstract class Tree {
  def maxPrevious: Float
}

case class Node(left: Tree, right: Tree) extends Tree {
  val maxPrevious = left.maxPrevious.max(right.maxPrevious)
}

case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

object LineOfSight extends LineOfSightInterface {

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    val length = input.length
    var i = 1
    var max = 0.0f
    output(0) = max
    while(i < length) 
      if (max < input(i)/i)
        max = input(i)/i
      output(i) = max
      i +=1
  }

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    var i = from
    var max = 0.0f
    while (i < until)
      if (max < input(i)/i)
        max = input(i)/i
      i+=1
    max
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int,
    threshold: Int): Tree = {
    if (end-from <= threshold) Leaf(from, end, upsweepSequential(input, from, end))
    else 
      val mid = from + (end-from)/2
      val (tL, tR) = parallel(upsweep(input, from, mid, threshold),
      upsweep(input, mid, end, threshold))
      Node(tL, tR)
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
    startingAngle: Float, from: Int, until: Int): Unit = {
    var max = startingAngle
    var i = from
    while (i < until)
      if (max < input(i)/i)
        max = input(i)/i
      output(i) = max
      i+=1
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepSequential` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
    tree: Tree): Unit = {
    def push(t: Tree, angle: Float): Unit = t match
      case Leaf(from: Int, until : Int, _) => downsweepSequential(input, output, angle, from, until)
      case Node(l: Tree, r: Tree) => parallel(downsweep(input, output, angle, l), 
      downsweep(input, output, Math.max(l.maxPrevious, angle), r))
    output(0) = 0
    push(tree, startingAngle)
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
    output(0) = 0.0f
    val tree = upsweep(input, 1, input.length, threshold)
    downsweep(input, output, 0f, tree)
  }
}
