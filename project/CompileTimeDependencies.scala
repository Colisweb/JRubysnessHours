import sbt._

object Versions {
  lazy final val scalaCompat = "2.3.1"
}

object CompileTimeDependencies {
  lazy final val scalaCompat = "org.scala-lang.modules" %% "scala-collection-compat" % Versions.scalaCompat
}
