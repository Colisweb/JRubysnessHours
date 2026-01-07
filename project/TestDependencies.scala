import sbt._

object TestVersions {
  lazy final val approval      = "1.3.2"
  lazy final val pprint        = "0.9.6"
  lazy final val scalacheck    = "2.12.0"
  lazy final val scalatest     = "3.2.19"
  lazy final val scalatestplus = "3.1.0.0-RC2"
}

object TestDependencies {
  lazy final val approval      = "com.colisweb"      %% "approvals-scala"          % TestVersions.approval      % Test
  lazy final val pprint        = "com.lihaoyi"       %% "pprint"                   % TestVersions.pprint        % Test
  lazy final val scalacheck    = "com.rallyhealth"   %% "scalacheck-ops_1-14"      % TestVersions.scalacheck    % Test
  lazy final val scalatest     = "org.scalatest"     %% "scalatest"                % TestVersions.scalatest     % Test
  lazy final val scalatestplus = "org.scalatestplus" %% "scalatestplus-scalacheck" % TestVersions.scalatestplus % Test
}
