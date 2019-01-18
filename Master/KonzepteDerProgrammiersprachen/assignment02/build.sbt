name := "Assignment 02"

// project properties
organization := "de.tuda.stg"
scalaVersion := "2.12.6"
version := "0.0.1"


// testing library
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4"

// run tests sequentially, so you know which one failed.
parallelExecution in Test := false

// more errorchecking
scalacOptions ++= Seq("-unchecked", "-deprecation")

// keys for uploading
serverUrl := "https://submission.st.informatik.tu-darmstadt.de/submit"
exercise := 2
course := "copl18"
