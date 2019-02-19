package instrumentation

import java.util.concurrent._;
import scala.concurrent.duration._
import scala.collection.mutable._
import Stats._

import java.util.concurrent.atomic.AtomicInteger

import pubsub.collection._

class SchedulableInternalBuffer[T](val size: Int, scheduler: Scheduler) extends InternalBuffer[T] {
  private val buffer = new Array[Option[T]](size)
  private val threadBuffer = new Array[Option[Int]](size) // Who last wrote in the array.

  def update(index: Int, elem: T): Unit = {
    scheduler.exec {
      buffer(index) = Some(elem)
      threadBuffer(index) = Some(scheduler.threadId)
    }(s"Write buffer($index) = $elem")
  }

  def apply(index: Int): T = scheduler.exec {
    buffer(index).fold {
      threadBuffer(index).fold {
        throw new Exception(s"buffer($index) was never set ! ")
      } { tid =>
        throw new Exception(s"buffer($index) was deleted by thread $tid ! ")
      }
    }(identity)
  }(s"Read buffer($index)")

  def delete(index: Int): Unit = {
    scheduler.exec {
      buffer(index) = None
      threadBuffer(index) = Some(scheduler.threadId)
    }(s"Delete buffer($index)")
  }
}

trait MockedInternals[T] { self: SchedulableBoundedBuffer[T] =>
  override val buffer = new SchedulableInternalBuffer[T](self.size, self.scheduler)

  var h: Int = 0
  var c: Int = 0

  override def head_=(i: Int) = scheduler.exec {
    h = i
  }(s"Write head  = $i")
  override def head: Int = scheduler.exec { h }(s"Read  head  -> $h")

  override def count_=(i: Int) = scheduler.exec {
    c = i
  }(s"Write count = $i")

  override def count: Int = scheduler.exec { c }(s"Read  count -> $c")
}

class SchedulableBoundedBuffer[T](val size: Int, val scheduler: Scheduler)
    extends BoundedBuffer[T](size) with MockedMonitor with MockedInternals[T] {

}

