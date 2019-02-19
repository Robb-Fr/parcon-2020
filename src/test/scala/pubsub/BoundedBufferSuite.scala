package pubsub

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.HashMap
import pubsub.collection._
import org.junit._
import org.junit.Assert.assertEquals

import instrumentation._
import instrumentation.Stats._
import TestHelper._
import TestUtils._

class BoundedBufferSuite {
  @Test def `Should work in a sequential setting`: Unit = {
    testSequential[(Int, Int, Int, Int)]{ sched =>
      val buffer = new SchedulableBoundedBuffer[Int](4, sched)
      buffer.put(1)
      buffer.put(2)
      buffer.put(3)
      buffer.put(4)
      (buffer.take(),
      buffer.take(),
      buffer.take(),
      buffer.take())
    }{ tuple =>
      (tuple == (1, 2, 3, 4), s"Expected (1, 2, 3, 4) got $tuple")
    }
  }

  @Test def `Should work when Thread 1: 'put(1)', Thread 2: 'take' and a buffer of size 1`: Unit = {
    testManySchedules(2, sched => {
      val prodCons = new SchedulableBoundedBuffer[Int](1, sched)
      (List(() => prodCons.put(1), () => prodCons.take()),
       args => (args(1) == 1, s"expected 1 your 'take' implementation returned ${args(1)}"))
    })
  }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(400 * 1000)
}
