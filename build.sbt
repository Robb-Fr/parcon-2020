course := "progfun1"
assignment := "example"
scalaVersion := "0.23.0-bin-20200211-5b006fb-NIGHTLY"
scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test

testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")
testSuite := "example.ListsSuite"
