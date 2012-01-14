import sbt._

object LinProgPluginDef extends Build {
  lazy val root = Project("plugins", file(".")) dependsOn(junitXmlListener)
  /* This is not published in a Maven repository, so we get it from GitHub directly */
  lazy val junitXmlListener = uri("git://github.com/ijuma/junit_xml_listener.git#cb0a6d40d44adc28ddbee00436bd814e65bf0487")
}



