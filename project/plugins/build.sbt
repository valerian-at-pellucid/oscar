
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.7.2")


// Jacoco4SBT (I put an earlier version to be compatible with SBT 0.11.2 and Scala 2.9.1)
libraryDependencies ++= Seq(
  "org.jacoco" % "org.jacoco.core" % "0.5.6.201201232323" artifacts(Artifact("org.jacoco.core", "jar", "jar")),
  "org.jacoco" % "org.jacoco.report" % "0.5.6.201201232323" artifacts(Artifact("org.jacoco.report", "jar", "jar")))

addSbtPlugin("de.johoop" % "jacoco4sbt" % "1.2.1")
