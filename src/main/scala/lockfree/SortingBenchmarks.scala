package lockfree

import scala.util.Random
import instrumentation.Stats._

object SortingBenchmarks extends App {

  val items = 2500
  val threads = 12

  val insertions = List.fill(items)(Random.nextInt)
  val deletions = Random.shuffle(insertions)
  val parInsertions = parBuckets(threads, insertions)_
  val parDeletions = parBuckets(threads, deletions)_

  private class SynchronizedSortedList extends SortedList {
    val lock = new Object
    def syncTraverse(stop: Int => Boolean) = lock.synchronized(super.findNodeWithPrev(stop))
    def syncInsert(e: Int) = lock.synchronized(super.insert(e))
    def syncDelete(e: Int) = lock.synchronized(super.delete(e))
  }

  val syncSeqTime = {
    val l = new SynchronizedSortedList
    withTime {
      insertions.foreach(l.syncInsert)
      deletions.foreach(l.syncDelete)
    }._2
  }

  val syncParTime = {
    val l = new SynchronizedSortedList
    withTime {
      val ops = parInsertions(l.syncInsert) ++ parDeletions(l.syncDelete)
      ops.foreach(_.start)
      ops.foreach(_.join)
    }._2
  }

  val fineSeqTime = {
    val l = new SortedList
    withTime {
      insertions.foreach(l.insert)
      deletions.foreach(l.delete)
    }._2
  }

  val fineParTime = {
    val l = new SortedList
    withTime {
      val ops = parInsertions(l.insert) ++ parDeletions(l.delete)
      ops.foreach(_.start)
      ops.foreach(_.join)
    }._2
  }

  println(
    s"""
      |***** Results *****
      |
      |synchronized:
      |  - sequential:  $syncSeqTime
      |  - parallel:    $syncParTime
      |  - speedup:     ${syncSeqTime / syncParTime}
      |
      |fine-grained:
      |  - sequential:  $fineSeqTime
      |  - parallel:    $fineParTime
      |  - speedup:     ${fineSeqTime / fineParTime}
      |
      |synchronized/fine-grained:
      |  - speedup:     ${syncParTime / fineParTime}
      |
    """.stripMargin)

  def parBuckets(threadCount: Int, items: List[Int])(op: Int => Any): List[Thread] =
    items.grouped(items.size / threadCount).toList.map { bucket =>
      new Thread {
        override def run(): Unit =
          bucket.foreach(op)
      }
    }
}
