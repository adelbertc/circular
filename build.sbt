import de.heikoseeberger.sbtheader.license.Apache2_0

lazy val buildSettings = List(
  organization := "com.adelbertc",
  licenses ++= List(
    ("Apache 2.0", url("https://www.apache.org/licenses/LICENSE-2.0")),
    ("BSD-Style" , url("https://opensource.org/licenses/BSD-3-Clause"))
  ),
  headers := Map(("scala", Apache2_0("2016", "Adelbert Chang"))),
  scalaVersion := "2.11.8",
  crossScalaVersions := List("2.10.6", scalaVersion.value),
  version := "0.1.0-SNAPSHOT"
)

val disabledReplOptions = Set("-Ywarn-unused-import")

val catsVersion = "0.7.2"
val specs2Version = "3.6"

lazy val commonSettings = List(
  scalacOptions ++= List(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  ) ++ scalaVersionFlags(scalaVersion.value),
  scalacOptions in (Compile, console) ~= { _.filterNot(disabledReplOptions.contains(_)) },
  scalacOptions in (Test, console) <<= (scalacOptions in (Compile, console)),
  resolvers ++= List(
    Resolver.bintrayRepo("scalaz", "releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  libraryDependencies ++= List(
    compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.0" cross CrossVersion.binary),
    "org.specs2"    %% "specs2-core"       % specs2Version % "test",
    "org.specs2"    %% "specs2-scalacheck" % specs2Version % "test"
  ) ++ scalaVersionDeps(scalaVersion.value)
)

lazy val circularSettings = buildSettings ++ commonSettings

lazy val circular =
  project.in(file(".")).
  settings(circularSettings).
  dependsOn(core, argonaut, laws, tests).
  aggregate(core, argonaut, laws, tests)

lazy val core =
  project.in(file("core")).
  settings(name := "circular-core").
  settings(description := "").
  settings(circularSettings).
  settings(
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-core" % catsVersion,
      "com.chuusai"   %% "shapeless" % "2.3.2"
    )
  )

lazy val argonaut =
  project.in(file("argonaut")).
  settings(name := "circular-argonaut").
  settings(description := "").
  settings(circularSettings).
  settings(
    libraryDependencies ++= List(
      "io.argonaut" %% "argonaut" % "6.2-M3"
    )
  ).
  dependsOn(core)

lazy val laws =
  project.in(file("laws")).
  settings(name := "circular-laws").
  settings(description := "").
  settings(circularSettings).
  settings(
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-laws" % catsVersion
    )
  ).
  dependsOn(core)

lazy val tests =
  project.in(file("tests")).
  settings(circularSettings).
  dependsOn(argonaut, core, laws)

def scalaVersionDeps(version: String): List[ModuleID] =
  if (version.startsWith("2.11")) List.empty
  else List(compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full))

def scalaVersionFlags(version: String): List[String] =
  if (version.startsWith("2.11")) List("-Ywarn-unused-import")
  else List.empty
