scalaVersion in ThisBuild := "2.11.8"
name := "raptiler"

version := "0.0.1"


mainClass in (Compile, run) := Some("raptorscript.raptiler.Cli")

//fork := true

connectInput in run := true
