import com.github.retronym.SbtOneJar._


name := "dreamer"

version := "0.1"

scalaVersion := "2.9.3"

scalaSource in Compile := baseDirectory.value / "src"

exportJars := true


// One-JAR settings - package into a single JAR including Scala libs

oneJarSettings

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"

