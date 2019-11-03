plugins {
  base
  id("org.mohme.gradle.elm-plugin" ) version "3.3.0"
}

group = "org.mohme"
version = "1.0-SNAPSHOT"

elm {
  sourceDir.set(project.file("src/main/elm"))
  targetModuleName.set("main.js")
  debug.set(true)
  optimize.set(false)
  executionDir.set(projectDir.canonicalPath)
  testExecutionDir.set(projectDir.canonicalPath)
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

  val minify = register("minify", Exec::class.java) {
    group = "build"
    description = "minification of produced javascript code"
    dependsOn(elmMake)

    this.onlyIf {
      project.elm.optimize.get()
    }

    executable = "bash"
    args("-c", "uglifyjs build/elm/main.js --compress 'pure_funcs=\"F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9\",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=build/elm/main.min.js")
  }
  assemble.get().dependsOn(minify.get())
}
