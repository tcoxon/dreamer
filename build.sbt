import com.github.retronym.SbtOneJar._


name := "dreamer"

version := "0.1"

//mainClass in Compile := Some("CLI")

mainClass in Compile := Some("AppletMain")

scalaVersion := "2.9.3"

scalaSource in Compile := baseDirectory.value / "src"

resourceDirectory in Compile := baseDirectory.value / "res"

exportJars := true

scalacOptions ++= Seq("-unchecked","-deprecation")

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"



// Unit testing:

libraryDependencies += "org.scalatest" % "scalatest_2.9.3" % "1.9.2" % "test"

scalaSource in Test := baseDirectory.value / "tests"



// One-JAR settings - package into a single JAR including Scala libs

oneJarSettings

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"


