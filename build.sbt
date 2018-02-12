name := course.value + "-" + assignment.value

scalaVersion := "2.11.7"
//scalaVersion := "2.12.4"

scalacOptions ++= Seq("-deprecation")

// grading libraries
libraryDependencies += "junit" % "junit" % "4.10" % Test
//libraryDependencies += "junit" % "junit" % "4.12" % Test

// for funsets
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
//libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"

// include the common dir
commonSourcePackages += "common"

courseId := "bRPXgjY9EeW6RApRXdjJPw"
