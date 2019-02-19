course := "reactive"
assignment := "actorbintree"

testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")
parallelExecution in Test := false

val akkaVersion = "2.6.0"

scalaVersion := "0.23.0-bin-20200211-5b006fb-NIGHTLY"

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-language:implicitConversions"
)

libraryDependencies ++= Seq(
  "com.typesafe.akka"        %% "akka-actor"         % akkaVersion,
  "com.typesafe.akka"        %% "akka-testkit"       % akkaVersion % Test,
  "com.novocode"             % "junit-interface"     % "0.11"      % Test
).map(_.withDottyCompat(scalaVersion.value))
testSuite := "actorbintree.BinaryTreeSuite"
