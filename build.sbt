import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME

enablePlugins(ScalaJSPlugin)

name := "Thread dump analyzer"
version := "5"

scalaVersion := "2.11.6" // or any other Scala version >= 2.10.2


libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.8.0",
  "com.lihaoyi" %%% "scalatags" % "0.5.2",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
)

emitSourceMaps := true

lazy val root = (project in file("."))
.enablePlugins(GitVersioning)
  .enablePlugins(BuildInfoPlugin)
  . settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion,
        BuildInfoKey.action("buildTime") {
          ZonedDateTime.now().format(ISO_OFFSET_DATE_TIME)
        }
        , "gitCommit" -> git.gitHeadCommit.value
      ),
    buildInfoPackage := "io.blep.tda"
  )

