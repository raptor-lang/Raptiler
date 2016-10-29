scalaVersion in ThisBuild := "2.11.8"
name := "raptorscript"

version := "0.0.1"


mainClass in (Compile, run) := Some("raptorscript.Cli")

//fork := true

connectInput in run := true
