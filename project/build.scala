import sbt._

object Builds extends Build {
  import Keys._

  lazy val buildSettings = Defaults.defaultSettings ++ Seq(
    version := "0.1.0",
    organization := "com.example",
    scalaVersion := "2.10.0-M7",
    resolvers ++= Seq("sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
      "sonatype releases"  at "http://oss.sonatype.org/content/repositories/releases")
  )

  lazy val root = Project("root", file("."),
    settings = buildSettings ++ Seq(
      name := "macro")) aggregate(library)
  lazy val library = Project("library", file("library"),
    settings = buildSettings ++ Seq(
      name := "macro-library",
      scalacOptions += "-language:experimental.macros",
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies += "com.eed3si9n" % "treehugger_bridge" % "0.2.3-SNAPSHOT" cross CrossVersion.full
    ))
  lazy val app = Project("app", file("app"),
    settings = buildSettings ++ Seq(
      name := "macro-app"
      ,scalacOptions += "-Ymacro-debug-lite"
    )) dependsOn(library)
}
