ThisBuild / organization := "com.colisweb"
ThisBuild / scalaVersion := "2.12.10"
ThisBuild / scalafmtOnCompile := true
ThisBuild / scalafmtCheck := true
ThisBuild / scalafmtSbtCheck := true

lazy val root = Project(id = "JRubysnessHours", base = file("."))
  .settings(moduleName := "root")
  .settings(noPublishSettings: _*)
  .aggregate(core, jruby)
  .dependsOn(core, jruby)

lazy val core =
  project
    .settings(moduleName := "JRubysnessHours")
    .settings(resolvers += Resolver.bintrayRepo("rallyhealth", "maven"))
    .settings(fork := true)
    .settings(libraryDependencies ++= approvalLibraries ++ scalacheckLibraries)

lazy val jruby =
  project
    .settings(moduleName := "JRubysnessHoursAdapter")
    .settings(fork := true)
    .settings(libraryDependencies ++= approvalLibraries)
    .dependsOn(core)

lazy val approvalLibraries = Seq(
  "org.scalatest"             %% "scalatest"       % "3.1.2",
  "com.lihaoyi"               %% "pprint"          % "0.5.9",
  "com.github.writethemfirst" %% "approvals-scala" % "1.0.0"
).map(_ % Test)

lazy val scalacheckLibraries = Seq(
  "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2",
  "com.rallyhealth" %% "scalacheck-ops_1-14" % "2.2.0"
).map(_ % Test)

/**
  * Copied from Cats
  */
def noPublishSettings = Seq(
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
