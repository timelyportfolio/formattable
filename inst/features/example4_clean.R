#devtools::install_github("renkun-ken/formattable@v0.2")

library(formattable)
library(htmltools)

fixedDigits <- function(x, n = 2) {
  formatC(x, digits = n, format = "f")
}

pFormatter <- formatter(
  "span",
  style = p ~ ifelse(p <= 0.05, style(font.weight = "bold"), NA),
  p ~ {
    p.formatted <- fixedDigits(p, 3)
    p.formatted <- gsub(x=p.formatted, pattern="^(-?)0", replacement="\\1")
    p.formatted[p < 0.001] <- "< .001"
    p.formatted
  }
)

estimateFormatter <- formatter(
  "span",
  style = ~ ifelse(
    p <= 0.05 & t < 0,
    "color:red",
    ifelse(p <= 0.05 & t > 0, "color:blue", NA)
  ),
  Estimate ~ fixedDigits(Estimate, 2)
)

# Add tiles to t-statistics.
tFormatter <- formatter(
  "span",
  style = x ~ style(
    display = "block",
    padding = "0 4px", `border-radius` = "4px",
    `background-color` = csscolor(gradient(abs(x),"white", "orange"))
  ),
  ~ fixedDigits(t, 2)
)

coef.matrix <- summary(lm(Sepal.Length ~ Species * Sepal.Width, iris))$coef
coef.df <- data.frame(coef.matrix, check.names=FALSE)
colnames(coef.df)[3:4] <- c("t","p")

tbl <- format_table(
  coef.df,
  col.names = c(
    "Estimate",
    "Standard<br/>Error",
    "<span style='font-style:italic;'>t</span>",
    "<span style='font-style:italic;'>p</span>"
  ),
  table.attr = paste0(
    'class = "table table-condensed"',
    'style = "margin:0px 50px 0px 50px; border-bottom: 2px solid; border-top: 2px solid;"',
    sep = " "
  ),
  align = c("l",rep("r",4)),
  caption = tagList(
    tags$h2(
      class=".h2",
      style="color:blue; text-align:center;line-height:0.75;",
      "Specially Formatted Table"
    ),
    tags$h4(
      class=".h4",
      style="color:green; text-align:right;line-height:0.75;",
      "Subtitle in Green"
    ),
    tags$caption(
      style="caption-side:bottom;font-style:italic;font-size:80%;",
      "This footer specifically designed
      to communicate important information.
      Since it is so important, it will of course
      extend over many lines.  In fact, on narrow tables,
      it might take >3.  On wide tables, it might only
      require one.  Feel free to adjust the width,
      and the importance and significance does not
      go away."
    )
  ),
  formatters = list(
    Estimate = estimateFormatter,
    "Std. Error" = x~digits(x,2),
    t = tFormatter,
    p = pFormatter
  )
)

browsable(
  attachDependencies(
    tagList(HTML(tbl)),
    list(
      rmarkdown::html_dependency_jquery(),
      rmarkdown::html_dependency_bootstrap("default")
    )
  )
)


# this is a really ugly way to return a htmlwidget
#  I will have to spend some time thinking through this.
# start by setting up a dummy formattable
ftw <- as.htmlwidget(formattable(data.frame()), width="80%")
# and replace the html with our formatted html from above
ftw$x$html <- HTML(tbl)
ftw
