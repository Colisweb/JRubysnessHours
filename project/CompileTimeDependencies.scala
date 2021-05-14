import sbt._

object Versions {
  lazy final val scalaCompat = "2.4.4"
}

object CompileTimeDependencies {
  lazy final val scalaCompat = "org.scala-lang.modules" %% "scala-collection-compat" % Versions.scalaCompat
}
