plugins {
  base
  id("org.mohme.gradle.elm-plugin" ) version "3.2.2"
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
  clean.configure {
    delete("${rootDir}/elm-stuff")
  }

  val copy = register<Copy>("copy") {
    group = "build"
    description = "Copy resources."
    from("src/main/resources")
    into("${buildDir}")
  }

  assemble.get().dependsOn(elmMake, copy)
  check.get().dependsOn(elmTest)
}
