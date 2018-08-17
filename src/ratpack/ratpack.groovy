@Grab('io.ratpack:ratpack-groovy:1.5.4')

import static ratpack.groovy.Groovy.ratpack

ratpack {
  handlers {
    files {
      dir "web-resources/public" indexFiles "index.html"
    }
  }
}