name := "Assignment 0"

// project properties
organization := "de.tuda.stg"
scalaVersion := "2.12.7"
version := "0.0.1"


// testing library
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5"

// more errorchecking
scalacOptions ++= Seq("-unchecked", "-deprecation")


// keys for uploading
serverUrl := "https://submission.st.informatik.tu-darmstadt.de/submit"
exercise := 0
course := "copl18"
