ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.8.1"

lazy val catsVersion = "2.13.0"
lazy val CatsCore = "org.typelevel" %% "cats-core" % catsVersion

lazy val circeVersion = "0.14.15"
lazy val CirceCore = "io.circe" %% "circe-core" % circeVersion
lazy val CirceGeneric = "io.circe" %% "circe-generic" % circeVersion
lazy val CirceParser = "io.circe" %% "circe-parser" % circeVersion

lazy val nettyVersion = "4.2.7.Final"
lazy val NettyAll = "io.netty" % "netty-all" % nettyVersion

lazy val zioVersion = "2.1.24"
lazy val Zio = "dev.zio" %% "zio" % zioVersion
lazy val ZioStreams = "dev.zio" %% "zio-streams" % zioVersion
lazy val ZioConcurrent = "dev.zio" %% "zio-concurrent" % zioVersion

lazy val zioPreludeVersion = "1.0.0-RC45"
lazy val ZioPrelude = "dev.zio" %% "zio-prelude" % zioPreludeVersion

/**
 * The project that defines the API.
 */
lazy val api = (project in file("api"))
  .settings(
    name := "wiz-lights-api",
    libraryDependencies ++= Seq(
      Zio,
      ZioStreams
    ),
    idePackagePrefix := Some("net.wayfarerx.wizlights")
  )

/**
 * The project that defines the backend services.
 */
lazy val backend = (project in file("backend"))
  .settings(
    name := "wiz-lights-backend",
    libraryDependencies ++= Seq(
      CatsCore,
      CirceCore,
      CirceGeneric,
      CirceParser,
      NettyAll,
      Zio,
      ZioStreams,
      ZioConcurrent,
      ZioPrelude
    ),
    idePackagePrefix := Some("net.wayfarerx.wizlights.backend")
  )
  .dependsOn(api)

/**
 * The project that aggregates all the other projects.
 */
lazy val root = (project in file("."))
  .settings(
    name := "wiz-lights",
    publish / skip := true
).aggregate(api, backend)
