import sbt._

object Versions {
  lazy final val scalaCompat = "2.4.1"
}

object CompileTimeDependencies {
  lazy final val scalaCompat = "org.scala-lang.modules" %% "scala-collection-compat" % Versions.scalaCompat
}
