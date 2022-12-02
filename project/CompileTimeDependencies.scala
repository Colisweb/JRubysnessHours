import sbt._

object Versions {
  lazy final val scalaCompat = "2.9.0"
}

object CompileTimeDependencies {
  lazy final val scalaCompat = "org.scala-lang.modules" %% "scala-collection-compat" % Versions.scalaCompat
}
