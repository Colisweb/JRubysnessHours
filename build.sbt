import CompileFlags._
import org.typelevel.scalacoptions.ScalacOptions

lazy val scala212               = "2.12.19"
lazy val scala213               = "2.13.13"
lazy val supportedScalaVersions = List(scala213, scala212)

ThisBuild / organization      := "com.colisweb"
ThisBuild / scalaVersion      := scala213
ThisBuild / scalafmtOnCompile := true
ThisBuild / scalafmtCheck     := true
ThisBuild / scalafmtSbtCheck  := true
ThisBuild / scalacOptions ++= crossScalacOptions(scalaVersion.value)
ThisBuild / pushRemoteCacheTo := Some(
  MavenCache("local-cache", baseDirectory.value / sys.env.getOrElse("CACHE_PATH", "sbt-cache"))
)

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = Project(id = "JRubysnessHours", base = file("."))
  .settings(moduleName := "root")
  .settings(noPublishSettings: _*)
  .aggregate(core, jruby)

lazy val core =
  project
    .settings(moduleName := "JRubysnessHours")
    .settings(fork := true)
    .settings(crossScalaVersions := supportedScalaVersions)
    .settings(Test / tpolecatExcludeOptions += ScalacOptions.warnNonUnitStatement)
    .settings(
      libraryDependencies ++= Seq(
        CompileTimeDependencies.scalaCompat
      ) ++ Seq(
        TestDependencies.approval,
        TestDependencies.scalatest,
        TestDependencies.scalatestplus,
        TestDependencies.scalacheck
      ),
      dependencyOverrides += TestDependencies.pprint
    )

lazy val jruby =
  project
    .settings(moduleName := "JRubysnessHoursAdapter")
    .settings(fork := true)
    .dependsOn(core % "test->test;compile->compile")
    .settings(Test / tpolecatExcludeOptions += ScalacOptions.warnNonUnitStatement)
    .settings(crossScalaVersions := supportedScalaVersions)
    .settings(dependencyOverrides += TestDependencies.pprint)

/** Copied from Cats
  */
def noPublishSettings =
  Seq(
    publish         := {},
    publishLocal    := {},
    publishArtifact := false
  )

// TODO: wip for debug logs
parallelExecution / test := false
