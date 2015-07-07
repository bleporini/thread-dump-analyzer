
enablePlugins(ScalaJSPlugin)

name := "Thread dump analyzer"

scalaVersion := "2.11.6" // or any other Scala version >= 2.10.2


libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.8.0",
  "com.lihaoyi" %%% "scalatags" % "0.5.2",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
)

emitSourceMaps := true

