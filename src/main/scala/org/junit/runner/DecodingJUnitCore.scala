package org.junit.runner

import collection.JavaConverters._
import java.util.ArrayList
import org.junit.internal.{JUnitSystem, RealSystem, TextListener}
import org.junit.runner.notification.Failure
import org.junit.runner.Result

class DecodingJUnitCore extends JUnitCore {
  // JUnitCore.runMain is unfortunatly private, so here it is, copy pasted
  def runMain(system: JUnitSystem, args: Array[String]): Result = {
    val classes = new ArrayList[Class[_]]()
    val missingClasses = new ArrayList[Failure]()
    args.foreach { each =>
      try {
          classes.add(Class.forName(each))
      } catch {
        case e: ClassNotFoundException =>
          println("Could not find class: " + each)
          val description = Description.createSuiteDescription(each)
          val failure = new Failure(description, e)
          missingClasses.add(failure)
      }
    }
    val listener = new DecodingTextListener(system)
    addListener(listener)
    val result = run(classes.toArray(new Array[Class[_]](0)): _*)
    missingClasses.asScala.foreach(result.getFailures().add)
    result
  }
}

object DecodingJUnitCore {
  def main(args: Array[String]): Unit = {
    val result = new DecodingJUnitCore().runMain(new RealSystem(), args)
    System.exit(if (result.wasSuccessful()) 0 else 1)
  }
}

class DecodingTextListener(system: JUnitSystem) extends TextListener(system) {
  override def printFailure(each: Failure, prefix: String): Unit = {
    println(prefix + ") " + scala.reflect.NameTransformer.decode(each.getTestHeader()))
    print(each.getTrace())
  }
}
