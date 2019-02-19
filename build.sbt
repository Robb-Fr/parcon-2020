course := "bigdata"
assignment := "wikipedia"

scalaVersion := "0.24.0-RC1"
scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")
libraryDependencies ++= Seq(
  "com.novocode" % "junit-interface" % "0.11" % Test,
  ("org.apache.spark" %% "spark-core" % "3.0.0-X1").withDottyCompat(scalaVersion.value),
)

// Contains Spark 3 snapshot built against 2.13: https://github.com/smarter/spark/tree/scala-2.13
resolvers += Resolver.bintrayRepo("smarter", "maven")

testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")

testSuite := "wikipedia.WikipediaSuite"

// Without forking, ctrl-c doesn't actually fully stop Spark
fork in run := true
fork in Test := true
