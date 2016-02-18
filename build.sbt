name := """StructuredQueryBuilder"""

version := "0.0.1"

scalaVersion := "2.11.7"

organization := "com.noriakihoriuchi"

libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.9.2",
  "org.scalatest" %% "scalatest" % "2.2.6" % Test
)

publishTo := Some(Resolver.file("com.noriakihoriuchi",file("build"))(Patterns(true, Resolver.mavenStyleBasePattern)))
