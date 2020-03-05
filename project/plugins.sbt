resolvers += Resolver.bintrayRepo("colisweb", "sbt-plugins")

addSbtPlugin("io.get-coursier"           % "sbt-coursier"  % "1.0.3")
addSbtPlugin("com.timushev.sbt"          % "sbt-updates"   % "0.5.0")
addSbtPlugin("com.geirsson"              % "sbt-scalafmt"  % "1.6.0-RC4")
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat"  % "0.1.11")
addSbtPlugin("org.scoverage"             % "sbt-scoverage" % "1.6.1")
addSbtPlugin("com.github.gseitz"         % "sbt-release"   % "1.0.12")
addSbtPlugin("org.foundweekends"         % "sbt-bintray"   % "0.5.5")
