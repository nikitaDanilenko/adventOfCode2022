val scala3Version = "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "adventOfCode2022",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "com.lihaoyi"   %% "pprint"     % "0.8.1",
      "org.typelevel" %% "cats-parse" % "0.3.8"
    )
  )
