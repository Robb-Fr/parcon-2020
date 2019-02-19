course := "parprog1"
assignment := "scalashop"

scalaVersion := "0.23.0-bin-20200211-5b006fb-NIGHTLY"
scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")
libraryDependencies ++= Seq(
  "com.storm-enroute" %% "scalameter-core" % "0.19",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0",
  "com.novocode" % "junit-interface" % "0.11" % Test
).map(_.withDottyCompat(scalaVersion.value))

testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")
testSuite := "scalashop.BlurSuite"
