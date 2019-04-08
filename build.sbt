ThisBuild / organization      := "com.colisweb"
ThisBuild / scalaVersion      := "2.12.8"
ThisBuild / scalafmtOnCompile := true
ThisBuild / scalafmtCheck     := true
ThisBuild / scalafmtSbtCheck  := true


lazy val root = Project(id = "JRubysnessHours", base = file("."))
  .settings(moduleName := "root")
  .settings(noPublishSettings: _*)
  .aggregate(core, jruby)
  .dependsOn(core, jruby)

lazy val core =
  project
    .settings(moduleName := "JRubysnessHours")
    .settings(
      libraryDependencies ++= Seq(
        "org.scalatest"  %% "scalatest"  % "3.0.7" % Test
      )
    )

lazy val jruby =
  project
    .settings(moduleName := "JRubysnessHoursAdapter")
    .dependsOn(core)

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
    credentials += Credentials(Path.userHome / ".bintray" / ".credentials"),
    licenses := Seq("Apache 2" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    homepage := Some(url("https://github.com/Colisweb/JRubysnessHours")),
    bintrayOrganization := Some("colisweb"),
    bintrayReleaseOnPublish := true,
    publishMavenStyle := true,
    pomExtra := (
      <scm>
        <url>git@github.com:Colisweb/JRubysnessHours.git</url>
        <connection>scm:git:git@github.com:Colisweb/JRubysnessHours.git</connection>
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