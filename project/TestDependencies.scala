import sbt._

object TestVersions {
  lazy final val approval = "1.2.1"
  lazy final val pprint =
    "0.5.9" // /!\ Ignored by scala-steward because 0.6.0 has different formatting in scala 2.12 and 2.13 !
  lazy final val scalacheck    = "2.6.0"
  lazy final val scalatest     = "3.2.9"
  lazy final val scalatestplus = "3.1.0.0-RC2"
}

object TestDependencies {
  lazy final val approval      = "com.colisweb"      %% "approvals-scala"          % TestVersions.approval      % Test
  lazy final val pprint        = "com.lihaoyi"       %% "pprint"                   % TestVersions.pprint        % Test
  lazy final val scalacheck    = "com.rallyhealth"   %% "scalacheck-ops_1-14"      % TestVersions.scalacheck    % Test
  lazy final val scalatest     = "org.scalatest"     %% "scalatest"                % TestVersions.scalatest     % Test
  lazy final val scalatestplus = "org.scalatestplus" %% "scalatestplus-scalacheck" % TestVersions.scalatestplus % Test
}
