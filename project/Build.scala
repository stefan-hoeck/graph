import sbt._
import Keys._

object BuildSettings {
  val buildOrganization = "graph"
  val buildVersion = "0.1.0-SNAPSHOT"
  val buildScalaVersion = "2.10.3"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    fork in run := true,
    javaOptions in run <++= (fullClasspath in Runtime) map { cp => Seq("-cp", sbt.Build.data(cp).mkString(":")) },
    exportJars := true,
    scalacOptions ++= Seq ("-deprecation", "-feature", "-language:postfixOps",
      "-language:higherKinds"),
    initialCommands in console := """
      import scalaz._, Scalaz._
      import graph._
    """
  )

} 

object Dependencies {
  val scalaz = "org.scalaz"
  val scalazV = "7.0.3"

  val scalaz_core = scalaz %% "scalaz-core" % scalazV
  val scalaz_effect = scalaz %% "scalaz-effect" % scalazV
  val scalaz_concurrent = scalaz %% "scalaz-concurrent" % scalazV
  val scalaz_iteratee = scalaz %% "scalaz-iteratee" % scalazV
  val scalaz_scalacheck = scalaz %% "scalaz-scalacheck-binding" % scalazV % "test"
  val spire = "org.spire-math" %% "spire" % "0.4.0"

  val scalacheck = "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"

  val caliper = "com.google.caliper" % "caliper" % "0.5-rc1"
}

object UtilBuild extends Build {
  import Dependencies._
  import BuildSettings._

  def addDeps (ds: ModuleID*) =
    BuildSettings.buildSettings ++
    Seq (libraryDependencies ++= ds) ++
    com.github.retronym.SbtOneJar.oneJarSettings

  lazy val chemf = Project (
    "graph",
    file("."),
    settings = addDeps (scalaz_core, scalaz_concurrent,
                        scalaz_effect, scalaz_iteratee,
                        scalaz_scalacheck, scalacheck,
                        caliper, spire)
  )
}

// vim: set ts=2 sw=2 et nowrap:
