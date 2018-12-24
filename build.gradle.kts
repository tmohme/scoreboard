import org.mohme.gradle.ElmMakeTask
import org.mohme.gradle.ElmPluginExtension

plugins {
  base
  id("org.mohme.gradle.elm-plugin" ) version "3.0.0"
}

group = "org.mohme"
version = "1.0-SNAPSHOT"

elm {
  sourceDir.set(project.file("src/main/elm"))
  targetModuleName.set("main.js")
  debug.set(true)
  optimize.set(false)
}

tasks {
  named("clean", Delete::class) {
    delete("${rootDir}/elm-stuff")
  }

  val copy = create("copy", Copy::class) {
    group = "build"
    description = "Copy resources."
    from("src/main/resources")
    into("${buildDir}")
  }

  named("assemble") {
    dependsOn("elmMake", copy)
  }
}
