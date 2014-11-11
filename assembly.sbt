import AssemblyKeys._

assemblySettings

jarName in assembly := "dreamer.jar"

test in assembly := {}

mainClass in assembly := Some("AppletMain")

