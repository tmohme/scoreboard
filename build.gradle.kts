import org.mohme.gradle.ElmMakeTask

plugins {
  base
  id("org.mohme.gradle.elm-plugin" ) version "2.1.0-SNAPSHOT"
}

group = "org.mohme"
version = "1.0-SNAPSHOT"

elm {
  setSourceDir(project.file("src/main/elm"))
  setTargetModuleName("main.js")
  setDebug(true)
  setOptimize(false)
}

tasks {
  getByName("clean", Delete::class) {
    delete("${rootDir}/elm-stuff")
  }

  val copy = create("copy", Copy::class) {
    group = "build"
    description = "Copy resources."
    from("src/main/resources")
    into("${buildDir}")
  }

  getByName("assemble") {
    dependsOn("elmMake", copy)
  }
}
