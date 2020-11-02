import CompileFlags._

lazy val scala212               = "2.12.12"
lazy val scala213               = "2.13.2"
lazy val supportedScalaVersions = List(scala213, scala212)

ThisBuild / organization := "com.colisweb"
ThisBuild / scalaVersion := scala213
ThisBuild / scalafmtOnCompile := true
ThisBuild / scalafmtCheck := true
ThisBuild / scalafmtSbtCheck := true
ThisBuild / scalacOptions ++= crossScalacOptions(scalaVersion.value)
Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = Project(id = "JRubysnessHours", base = file("."))
  .settings(moduleName := "root")
  .settings(noPublishSettings: _*)
  .aggregate(core, jruby)

lazy val core =
  project
    .settings(moduleName := "JRubysnessHours")
    .settings(resolvers += Resolver.bintrayRepo("rallyhealth", "maven"))
    .settings(fork := true)
    .settings(crossScalaVersions := supportedScalaVersions)
    .settings(
      libraryDependencies ++= Seq(
        CompileTimeDependencies.scalaCompat
      ) ++ Seq(
        TestDependencies.approval,
        TestDependencies.pprint,
        TestDependencies.scalatest,
        TestDependencies.scalatestplus,
        TestDependencies.scalacheck
      )
    )

lazy val jruby =
  project
    .settings(moduleName := "JRubysnessHoursAdapter")
    .settings(fork := true)
    .dependsOn(core % "test->test;compile->compile")
    .settings(crossScalaVersions := supportedScalaVersions)

/**
  * Copied from Cats
  */
def noPublishSettings =
  Seq(
    publish := {},
    publishLocal := {},
    publishArtifact := false
  )

inThisBuild(
  List(
    licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    homepage := Some(url("https://gitlab.com/colisweb-open-source/scala/JRubysnessHours")),
    bintrayOrganization := Some("colisweb"),
    resolvers += Resolver.bintrayRepo("writethemfirst", "maven"),
    scalacOptions += "-Yresolve-term-conflict:object",
    publishMavenStyle := true,
    pomExtra := (
      <scm>
        <url>https://gitlab.com/colisweb-open-source/scala/JRubysnessHours</url>
        <connection>scm:git:git@gitlab.com:colisweb-open-source/scala/JRubysnessHours.git</connection>
      </scm>
        <developers>
          <developer>
            <id>FlorianDoublet</id>
            <name>Florian Doublet</name>
            <url>https://www.colisweb.com</url>
          </developer>
          <developer>
            <id>cverdier</id>
            <name>Cyril Verdier</name>
            <url>https://www.colisweb.com</url>
          </developer>
        </developers>
    )
  )
)

// TODO: wip for debug logs
parallelExecution in test := false
