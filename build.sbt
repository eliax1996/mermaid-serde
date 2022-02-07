name := "mermaid-serde"

version := "0.1"

scalaVersion := "2.13.7"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.3.3"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"
libraryDependencies ++= Seq(
  "eu.timepit" %% "refined" % "0.9.28"
//  "eu.timepit" %% "refined-cats" % "0.9.28", // optional
//  "eu.timepit" %% "refined-eval" % "0.9.28", // optional, JVM-only
//  "eu.timepit" %% "refined-jsonpath" % "0.9.28", // optional, JVM-only
//  "eu.timepit" %% "refined-pureconfig" % "0.9.28", // optional, JVM-only
//  "eu.timepit" %% "refined-scalacheck" % "0.9.28", // optional
//  "eu.timepit" %% "refined-scalaz" % "0.9.28", // optional
//  "eu.timepit" %% "refined-scodec" % "0.9.28", // optional
//  "eu.timepit" %% "refined-scopt" % "0.9.28", // optional
//  "eu.timepit" %% "refined-shapeless" % "0.9.28" // optional
)

libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.3.3" // SBT
libraryDependencies += "com.lihaoyi" %% "pprint" % "0.7.1" % Test
