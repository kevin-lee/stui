import sbtcrossproject.CrossProject

ThisBuild / organization     := props.Org
ThisBuild / organizationName := "Kevin's Code"
ThisBuild / scalaVersion     := props.Scala3Version
ThisBuild / version          := props.Version

ThisBuild / licenses   := List(License.MIT)
ThisBuild / developers := List(
  Developer(
    props.GitHubUsername,
    "Kevin Lee",
    "kevin.code@kevinlee.io",
    url(s"https://github.com/${props.GitHubUsername}"),
  )
)
ThisBuild / homepage   := Some(url(s"https://github.com/${props.GitHubUsername}/${props.RepoName}"))
ThisBuild / scmInfo    :=
  Some(
    ScmInfo(
      url(s"https://github.com/${props.GitHubUsername}/${props.RepoName}"),
      s"git@github.com:${props.GitHubUsername}/${props.RepoName}.git",
    )
  )

ThisBuild / semanticdbEnabled                            := true
ThisBuild / semanticdbVersion                            := scalafixSemanticdb.revision
ThisBuild / scalafixDependencies += "com.github.xuwei-k" %% "scalafix-rules" % props.ScalafixRulesVersion
ThisBuild / scalafixConfig                               := Some(file(".scalafix.conf"))

lazy val stui = (project in file("."))
  .settings(name := props.ProjectName)
  .settings(noPublish)
  .settings(noDoc)
  .aggregate(
    unicodeJvm,
    unicodeJs,
    unicodeNative,
    coreJvm,
    coreJs,
    coreNative,
    testkitJvm,
    testkitJs,
    testkitNative,
    widgetsJvm,
    widgetsJs,
    widgetsNative,
    terminalJvm,
    terminalJs,
    terminalNative,
    examplesJvm,
    examplesNative,
  )

lazy val unicode       = module("stui-unicode", crossProject(JVMPlatform, JSPlatform, NativePlatform))
lazy val unicodeJvm    = unicode.jvm
lazy val unicodeJs     = unicode.js.settings(jsSettings)
lazy val unicodeNative = unicode.native.settings(nativeSettings)

lazy val core       = module("stui-core", crossProject(JVMPlatform, JSPlatform, NativePlatform))
  .settings(
    /* kittens' coproduct derivation needs more inlining depth than the compiler's default (32) for enums above about 22 variants,
     * and `KeyCode` has 26. The limit applies only where the `derives` clause is compiled, so users of the published instances do not
     * need it. */
    scalacOptions += "-Xmax-inlines:64",
    libraryDependencies ++= List(
      libs.refined4sCore.value,
      libs.refined4sCats.value,
      libs.catsCore.value,
      libs.kittens.value,
      libs.extrasRender.value,
    ),
  )
  .dependsOn(unicode)
lazy val coreJvm    = core.jvm
lazy val coreJs     = core.js.settings(jsSettings)
lazy val coreNative = core.native.settings(nativeSettings)

lazy val testkit       = module("stui-testkit", crossProject(JVMPlatform, JSPlatform, NativePlatform))
  .settings(
    libraryDependencies ++= libs.hedgehogLibsForTestkit.value ++ List(libs.hedgehogExtraRefined4s.value)
  )
  .dependsOn(core)
lazy val testkitJvm    = testkit.jvm
lazy val testkitJs     = testkit.js.settings(jsSettings)
lazy val testkitNative = testkit.native.settings(nativeSettings)

lazy val widgets       = module("stui-widgets", crossProject(JVMPlatform, JSPlatform, NativePlatform))
  .dependsOn(core, testkit % Test)
lazy val widgetsJvm    = widgets.jvm
lazy val widgetsJs     = widgets.js.settings(jsSettings)
lazy val widgetsNative = widgets.native.settings(nativeSettings)

lazy val terminal       = module("stui-terminal", crossProject(JVMPlatform, JSPlatform, NativePlatform))
  .dependsOn(core, testkit % Test)
lazy val terminalJvm    = terminal
  .jvm
  .settings(
    libraryDependencies += libs.jna
  )
lazy val terminalJs     = terminal.js.settings(jsSettings)
lazy val terminalNative = terminal.native.settings(nativeSettings)

lazy val examples       = module("examples", crossProject(JVMPlatform, NativePlatform))
  .settings(noPublish)
  .settings(noDoc)
  .dependsOn(widgets, terminal)
/* The demo owns the terminal, so it must run in its own process with standard input connected (an in-process `run` shares sbt's
 * standard input). Alternative launches: `java -cp` over `sbt "export examplesJVM/Runtime/fullClasspath"`, and for Native
 * `sbt examplesNative/nativeLink` then `modules/examples/native/target/scala-3.3.8/examples`. */
lazy val examplesJvm    = examples
  .jvm
  .settings(
    run / fork           := true,
    run / connectInput   := true,
    run / outputStrategy := Some(StdoutOutput),
  )
lazy val examplesNative = examples.native.settings(nativeSettings)

lazy val props =
  new {
    val Org = "io.kevinlee"

    val GitHubUsername = "kevin-lee"

    val ProjectName = "stui"
    val RepoName    = "stui"

    val Scala3Version = "3.3.8"

    val Version = "0.1.0-SNAPSHOT"

    val Refined4sVersion = "1.21.0"

    val CatsVersion = "2.13.0"

    val KittensVersion = "3.5.0"

    val ExtrasVersion = "0.56.0"

    val HedgehogVersion = "0.14.0"

    val HedgehogExtraVersion = "0.24.0"

    val JnaVersion = "5.19.1"

    val ScalafixRulesVersion = "0.6.29"

    val IncludeTest = "compile->compile;test->test"
  }

lazy val libs =
  new {
    lazy val refined4sCore = Def.setting("io.kevinlee" %%% "refined4s-core" % props.Refined4sVersion)
    lazy val refined4sCats = Def.setting("io.kevinlee" %%% "refined4s-cats" % props.Refined4sVersion)

    lazy val catsCore = Def.setting("org.typelevel" %%% "cats-core" % props.CatsVersion)
    lazy val kittens  = Def.setting("org.typelevel" %%% "kittens" % props.KittensVersion)

    lazy val extrasRender = Def.setting("io.kevinlee" %%% "extras-render" % props.ExtrasVersion)

    lazy val hedgehogLibs = Def.setting(
      List(
        "qa.hedgehog" %%% "hedgehog-core"   % props.HedgehogVersion,
        "qa.hedgehog" %%% "hedgehog-runner" % props.HedgehogVersion,
        "qa.hedgehog" %%% "hedgehog-sbt"    % props.HedgehogVersion,
      )
    )

    /* stui-testkit exposes hedgehog generators, so core and runner are Compile dependencies there. */
    lazy val hedgehogLibsForTestkit = Def.setting(
      List(
        "qa.hedgehog" %%% "hedgehog-core"   % props.HedgehogVersion,
        "qa.hedgehog" %%% "hedgehog-runner" % props.HedgehogVersion,
      )
    )

    /* 0.24.0 is the first version whose Native artifact is free of scala-native-crypto (its `genUuid` is a pure generator). */
    lazy val hedgehogExtraRefined4s = Def.setting("io.kevinlee" %%% "hedgehog-extra-refined4s" % props.HedgehogExtraVersion)

    lazy val jna = "net.java.dev.jna" % "jna" % props.JnaVersion
  }

lazy val jsSettings: Seq[Setting[_]] = List(
  Test / fork := false
)

lazy val nativeSettings: Seq[Setting[_]] = List(
  Test / fork := false
)

/* Sources shared by the JVM and Native platforms only (the blocking event source, design doc 6.3, cannot exist on JS) live in
 * `modules/<name>/jvm-native/src/{main,test}/scala`. sbt-crossproject 1.4.0 adds that directory to both platform projects on its
 * own (like `js-jvm` and `js-native`), so no setting is needed here. */

def module(projectName: String, crossProject: CrossProject.Builder): CrossProject =
  crossProject
    .in(file(s"modules/$projectName"))
    .settings(
      name                              := projectName,
      scalacOptions ++= List("-no-indent", "-explain"),
      wartremoverErrors ++= Warts.allBut(Wart.Any, Wart.Nothing, Wart.ImplicitParameter),
      Compile / console / scalacOptions :=
        (console / scalacOptions)
          .value
          .filterNot(option => option.contains("wartremover") || option.contains("import")),
      Test / console / scalacOptions    :=
        (console / scalacOptions)
          .value
          .filterNot(option => option.contains("wartremover") || option.contains("import")),
      libraryDependencies ++= libs.hedgehogLibs.value.map(_ % Test),
    )
