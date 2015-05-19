name := """StrategiesGeneratorNew"""

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies += "org.specs2" %% "specs2-core" % "3.6" % "test"

libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.7.11"

libraryDependencies += "com.lihaoyi" %% "scalarx" % "0.2.8"

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"


// Uncomment to use Akka
libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.3"
