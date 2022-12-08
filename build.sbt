ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "adventOfCode2022",
    libraryDependencies ++= Seq(
      "com.lihaoyi"   %% "pprint"         % "0.8.0",
      "org.typelevel" %% "spire"          % "0.18.0-M2",
      "dev.optics"    %% "monocle-unsafe" % "3.1.0",
      "org.typelevel" %% "cats-parse"     % "0.3.8",
      "org.typelevel" %% "cats-core"      % "2.8.0"
    )
  )
