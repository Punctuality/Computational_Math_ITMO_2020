ThisBuild / scalaVersion := "2.13.1"
ThisBuild / version := "0.1.0"


ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-language:existentials",
  "-language:higherKinds",
  "-language:postfixOps",
  "-language:implicitConversions",
  "-Xlint",
  "-Xlint:deprecation",
//  "-Xfatal-warnings"
)

lazy val `compmath` = (project in file(".")) aggregate(
  `compmath-core`,
  `compmath-lab1`,
  `compmath-lab2`
)

lazy val `compmath-core` = (project in file("core"))
  .settings(
    name := "core",
    libraryDependencies ++= Seq()
  )

lazy val `compmath-lab1` = (project in file("lab1"))
  .settings(
    name := "lab1",
    libraryDependencies ++= Seq()
  )
  .dependsOn(`compmath-core`)

lazy val `compmath-lab2` = (project in file("lab2"))
  .settings(
    name := "lab2",
    libraryDependencies ++= Seq()
  )
  .dependsOn(`compmath-core`)