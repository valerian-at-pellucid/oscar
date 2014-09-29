package oscar

import sbt._
import sbt.Keys._


object Resolvers {
  val typesafe = Seq(
    "Typesafe repository snapshots" at "http://repo.typesafe.com/typesafe/snapshots/",
    "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/"
  )

  val resolversList = typesafe
}

object Dependencies {
  import sbt._


  val jfreechart = "org.jfree" % "jfreechart" % "1.0.14"
  val swingx     = "org.swinglabs" % "swingx" % "1.6.1"
  val swingxWS   = "org.swinglabs" % "swingx-ws" % "1.0"

  val jsci = "net.sf.jsci" % "jsci" % "1.2"

  val junit = "junit" % "junit" % "4.8.1"
  val scalaTest  = "org.scalatest" %% "scalatest" % "2.2.1"
  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.11.5"

  val testDependencies = Seq(
    scalaTest % "test",
    scalaCheck % "test",
    junit % "test"
  )

  val oscarVisualDependencies = Seq(
    jfreechart,
    swingx,
    swingxWS
  )
}

object BuildSettings {
  val buildOrganization = "oscar"
  val buildVersion = "1.1.0.1"
  val buildScalaVersion = "2.11.2"

  val buildSettings = Defaults.defaultSettings ++ Seq(
    resolvers ++= Resolvers.resolversList,
    organization := buildOrganization,
    version := buildVersion,
    licenses += ("LGPL-3.0", url("http://www.gnu.org/licenses/lgpl.html")),
    //scalacOptions in Compile ++= Seq("-encoding", "UTF-8", "-deprecation", "-feature", "-unchecked", "-Xdisable-assertions"),
    //testOptions in Test <+= (target in Test) map {
    //  t => Tests.Argument(TestFrameworks.ScalaTest, "junitxml(directory=\"%s\")" format (t / "test-reports")) },
    parallelExecution in Test := false,
    fork in Test := true,
    //javaOptions in Test += "-Djava.library.path=../lib:../lib/" + osNativeLibDir,
    //unmanagedBase <<= baseDirectory { base => base / "../lib/" }, // unfortunately does not work
    //unmanagedClasspath in Compile <+= (baseDirectory) map { bd => Attributed.blank(bd / "../lib/") },
    scalaVersion := "2.11.2",
    crossScalaVersions := Seq("2.11.2", "2.10.4"),
    libraryDependencies := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        // if scala 2.11+ is used, add dependency on scala-xml module
        case Some((2, scalaMajor)) if scalaMajor >= 11 =>
          Seq(
            "org.scala-lang.modules" %% "scala-xml" % "1.0.2",
            "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
            "org.scala-lang.modules" %% "scala-swing" % "1.0.1")
        case _ =>
          // or just libraryDependencies.value if you don't depend on scala-swing
          Seq("org.scala-lang" % "scala-swing" % scalaVersion.value)
      }
    }
  ) ++ bintray.Plugin.bintrayPublishSettings


  lazy val scalaModuleDependencies = buildSettings ++ Seq (
    scalaVersion := "2.11.1"


  )
}


object OscarBuild2 extends Build {

  lazy val oscar = Project(
    id = "oscar",
    base = file("."),
    settings = BuildSettings.buildSettings,
    aggregate = Seq(oscarVisual, oscarCp, oscarCbls, /*oscarLinprog,*/ oscarDes, oscarDfo))

  lazy val oscarCbls = Project(
    id = "oscar-cbls",
    base = file("oscar-cbls"),
    settings = BuildSettings.buildSettings ++ Seq(
      libraryDependencies ++= Dependencies.testDependencies
    ),
    dependencies = Seq(oscarVisual))

  lazy val oscarCp = Project(
    id = "oscar-cp",
    base = file("oscar-cp"),
    settings = BuildSettings.scalaModuleDependencies ++ Seq(
      libraryDependencies ++= Dependencies.testDependencies
    ),
    dependencies = Seq(oscarAlgo, oscarVisual))

  lazy val oscarDes = Project(
    id = "oscar-des",
    base = file("oscar-des"),
    settings = BuildSettings.buildSettings ++ Seq(
      libraryDependencies ++= Seq(Dependencies.jsci)
    ),
    dependencies = Seq(oscarInvariants))

  lazy val oscarDfo = Project(
    id = "oscar-dfo",
    base = file("oscar-dfo"),
    settings = BuildSettings.buildSettings ++ Seq(
      libraryDependencies ++= Dependencies.testDependencies
    ),
    dependencies = Seq(oscarAlgebra, oscarVisual, oscarAlgo))

  /*
  lazy val oscarLinprog = Project(
    id = "oscar-linprog",
    base = file("oscar-linprog"),
    settings = BuildSettings.buildSettings ++ Seq(
      libraryDependencies ++= Seq(Dependencies.glpkJava)
    ),
    dependencies = Seq(oscarAlgebra)
  )
*/

  lazy val oscarAlgo = Project(
    id = "oscar-algo",
    settings = BuildSettings.buildSettings,
    base = file("oscar-algo"),
    dependencies= Seq(oscarUtil,oscarVisual))

  lazy val oscarVisual = Project(
    id = "oscar-visual",
    settings = BuildSettings.scalaModuleDependencies ++ Seq(
      libraryDependencies ++= Dependencies.oscarVisualDependencies
    ),
    base = file("oscar-visual"),
    dependencies= Seq(oscarUtil))

  lazy val oscarInvariants = Project(
    id = "oscar-invariants",
    settings = BuildSettings.buildSettings,
    base = file("oscar-invariants"))

  lazy val oscarAlgebra = Project(
    id = "oscar-algebra",
    settings = BuildSettings.buildSettings,
    base = file("oscar-algebra"))

  lazy val oscarUtil = Project(
    id = "oscar-util",
    settings = BuildSettings.scalaModuleDependencies,
    base = file("oscar-util")
  )

}