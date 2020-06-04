import sbt.Keys.mainClass

ThisBuild / scalaVersion := "2.13.1"
ThisBuild / version := "0.4.0"

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
  core,
  lab1,
  lab2,
  lab3,
  lab4,
  lab5
)

lazy val core = (project in file("core"))
  .settings(
    name := "core",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.0.0",
      "org.typelevel" %% "cats-effect" % "2.0.0",
      "org.scijava" % "jep" % "2.4.2",
      "com.github.blemale" %% "scaffeine" % "3.1.0" % "compile"
    )
  )

lazy val lab1 = (project in file("lab1"))
  .settings(
    name := "lab1",
    libraryDependencies ++= Seq()
  )
  .dependsOn(core)

lazy val lab2 = (project in file("lab2"))
  .settings(
    name := "lab2",
    libraryDependencies ++= Seq()
  )
  .dependsOn(core)

lazy val lab3 = (project in file("lab3"))
  .settings(
    name := "lab3",
    mainClass in assembly := Some("lab3.Lab3Main"),
    assemblyJarName in assembly := "lab3.jar"
  )
  .dependsOn(
    core,
    lab1
  )

lazy val lab4 = (project in file("lab4"))
  .settings(
    name := "lab4",
    mainClass in assembly := Some("lab4.Lab4Main"),
    assemblyJarName in assembly := "lab4.jar"
  )
  .dependsOn(
    core
  )

lazy val lab5 = (project in file("lab5"))
  .settings(
    name := "lab5",
    mainClass in assembly := Some("lab5.Lab5Main"),
    assemblyJarName in assembly := "lab5.jar"
  )
  .dependsOn(
    core,
    lab2,
    lab4
  )