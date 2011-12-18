resolvers += Resolver.url("Typesafe repository - Snapshots", new java.net.URL("http://typesafe.artifactoryonline.com/typesafe/ivy-snapshots/"))(Patterns(false, "[organisation]/[module]/[revision]/jars/[artifact].[ext]"))

resolvers += Resolver.url("Typesafe repository - Releases", new java.net.URL("http://typesafe.artifactoryonline.com/typesafe/ivy-releases/"))(Patterns(false, "[organisation]/[module]/[revision]/jars/[artifact].[ext]"))

sbtPlugin := true


name := "Disassembler"

version := "0.1"

organization := "com.scalalabs"

// append -deprecation to the options passed to the Scala compiler
scalacOptions += "-deprecation"

// set the Scala version used for the project
scalaVersion := "2.9.1"

scalacOptions ++= Seq("-unchecked", "-deprecation")


// Add a single dependency
libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.6.1"

libraryDependencies += "junit" % "junit" % "4.10" 


libraryDependencies ++= Seq(
  "org.jacoco" % "org.jacoco.core" % "0.5.3.201107060350" artifacts(Artifact("org.jacoco.core", "jar", "jar")),
  "org.jacoco" % "org.jacoco.report" % "0.5.3.201107060350" artifacts(Artifact("org.jacoco.report", "jar", "jar")))

seq(jacoco.settings : _*)

jacoco.reportTitle := "Scala Disassembler"

// Exclude backup files by default.  This uses ~=, which accepts a function of
//  type T => T (here T = FileFilter) that is applied to the existing value.
// A similar idea is overriding a member and applying a function to the super value:
//  override lazy val defaultExcludes = f(super.defaultExcludes)
//
//defaultExcludes ~= (filter => filter || "*~")

///*  Some equivalent ways of writing this:
//defaultExcludes ~= (_ || "*~")
//defaultExcludes ~= ( (_: FileFilter) || "*~")
//defaultExcludes ~= ( (filter: FileFilter) => filter || "*~")
//*/

// Use the project version to determine the repository to publish to.
publishTo <<= version { (v: String) =>
  if(v endsWith "-SNAPSHOT")
    Some(ScalaToolsSnapshots)
  else
    Some(ScalaToolsReleases)
}