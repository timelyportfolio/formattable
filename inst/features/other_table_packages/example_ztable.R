library(ztable)
library(htmltools)

browsable(
  attachDependencies(
    tagList(
      HTML(capture.output(
        ztable(
          lm(Sepal.Length ~ Species * Sepal.Width, iris),
          type="html"
        )
      )),
      tags$script("$('table').addClass('table table-condensed table-hover').css('font-family','inherit')")
    ),
    list(
      rmarkdown::html_dependency_jquery(),
      rmarkdown::html_dependency_bootstrap("default")
    )
  )
)
