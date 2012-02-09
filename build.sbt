import AssemblyKeys._
name := "scampi"

version := "1.0"

organization := "nside"

scalaVersion := "2.9.1"

autoCompilerPlugins := true

libraryDependencies <<= (scalaVersion, libraryDependencies) { (ver, deps) =>
    deps :+ compilerPlugin("org.scala-lang.plugins" % "continuations" % ver)
}

scalacOptions += "-P:continuations:enable"

seq(assemblySettings: _*)

jarName in assembly := "scampi.jar"


test in assembly := {}


libraryDependencies += "org.scalatest" % "scalatest" % "1.4.RC2"


libraryDependencies += "com.novocode" % "junit-interface" % "0.7" % "test->default"


testListeners <<= target.map(t => Seq(new eu.henkelmann.sbt.JUnitXmlTestsListener(t.getAbsolutePath)))


//mainClass in (Compile, run) := Some("main.scala.scampi.dfo.examples.Rosenbrock2D")


parallelExecution in Test := false


