lazy val compilerOptions = Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Xfuture",
  "-Xlint"
)

val circeVersion = "0.5.1"

lazy val macroSettings = Seq(
  libraryDependencies += "org.scalameta" %% "scalameta" % "1.1.0",
  addCompilerPlugin(
    "org.scalameta" % "paradise" % "3.0.0-M5" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise"
)

lazy val commonSettings = Seq(
  triggeredMessage in ThisBuild := Watched.clearWhenTriggered,
  scalacOptions in (Compile, console) := compilerOptions :+ "-Yrepl-class-based",
  testOptions in Test += Tests.Argument("-oD")
)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishMavenStyle := true,
  publishArtifact := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  publishArtifact in Test := false,
  licenses := Seq(
    "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  homepage := Some(url("https://github.com/olafurpg/meta-config")),
  autoAPIMappings := true,
  apiURL := Some(url("https://github.com/olafurpg/meta-config")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/olafurpg/meta-config"),
      "scm:git:git@github.com:olafurpg/meta-config.git"
    )
  ),
  pomExtra :=
    <developers>
      <developer>
        <id>olafurpg</id>
        <name>Ólafur Páll Geirsson</name>
        <url>https://geirsson.com</url>
      </developer>
    </developers>
)

lazy val noPublish = Seq(
  publish := {},
  publishLocal := {}
)

lazy val buildSettings = Seq(
  organization := "ch.epfl.scala",
  // See core/src/main/scala/ch/epfl/scala/Versions.scala
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.11.8",
  updateOptions := updateOptions.value.withCachedResolution(true)
)

lazy val allSettings = commonSettings ++ buildSettings ++ publishSettings

lazy val macros = project
  .settings(allSettings: _*)
  .settings(macroSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
  )
