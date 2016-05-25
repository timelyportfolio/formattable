# from @renkun-ken html-table branch
#  https://github.com/renkun-ken/formattable/blob/html-table/R/html_table.R


bootstrap_dependency <- function(){
  htmltools::htmlDependency(
    name = "bootstrap",
    version = "3.3.6",
    src = c(href="http://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/"),
    script = "js/bootstrap.min.js",
    stylesheet = "css/bootstrap.min.css"
  )
}


#  added colhead style
html_table <- function(
  x, align = "left", caption = NULL,
  table_class = NULL, row_class = c("odd", "even"),
  colhead_style = "", include_rownames = FALSE
) {
  if(include_rownames){
    x <- data.frame(
      rownames(x),
      x,
      check.rows=FALSE,
      check.names=FALSE,
      stringsAsFactors=FALSE
    )
    colnames(x)[1] <- ""
  }
  row_counter <- -1L
  tag("table", list(
    class = table_class,
    if (is.null(caption) || is.na(caption)) NULL
    else tag("caption", list(caption)),
    tag("thead", list(
      tag("tr", c(class = "header",
                  .mapply(function(column, align, style) {
                    tag("th", list(align = align, style = style, style = paste0("text-align:", align) , HTML(column)))
                  }, list(colnames(x), align, colhead_style), NULL)))
    )),
    tag("tbody", .mapply(function(...) {
      row_counter <<- row_counter + 1L
      cells <- unname(list(...))
      tag("tr", c(class = row_class[row_counter %% length(row_class) + 1L],
                  .mapply(function(align, value) {
                    tag("td", list(align = align, HTML(value)))
                  }, list(align, cells), NULL)))
    }, x, NULL))
  ))
}


library(formattable)

# Print with n digits of precision
fixedDigits <- function(x, n = 2) {
  formatC(x, digits = n, format = "f")
}

# Make p-values nice.
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

# Make estimates nice.
estimateFormatter <- formatter(
  "span",
  style = ~ ifelse(
    p <= 0.05 & t < 0,
    "color:red",
    ifelse(
      p <= 0.05 & t > 0,
      "color:blue",
      NA
    )
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

# Example
coef.matrix <- summary(lm(Sepal.Length ~ Species * Sepal.Width, iris))$coef
coef.df <- as.data.frame(coef.matrix)
rownames(coef.df)[2] = "A label with some spaces"
colnames(coef.df)[2:4] <- c("Standard<br>Error", "t", "p")
ft <- formattable(
  coef.df,
  list(
    Estimate = estimateFormatter,
    "Standard<br>Error" = x~digits(x,2),
    "p" = pFormatter,
    "t" = tFormatter
  )
)
ft



# play with the html_table function
#  without bare html and not bootstrap
library(htmltools)
browsable(
  html_table(
    as.htmldf(ft),
    include_rownames=TRUE,
    colhead_style=paste0("vertical-align:bottom;",c("","","","font-style:italic;","font-style:italic;")),
    align=c("left",rep("right",4))
  )
)


tbl <- html_table(
  as.htmldf(ft),
  table_class="table table-condensed table-striped",
  include_rownames=TRUE,
  colhead_style=paste0("vertical-align:bottom;",c("","","","font-style:italic;","font-style:italic;")),
  align=c("left",rep("right",4)),
  caption=tagList(
    tags$caption(
      style="caption-side:top;",
      tags$h2(
        style="color:blue; text-align:center;",
        "Specially Formatted Table"
      ),
      tags$h4(
        style="color:green; text-align:right;",
        "Subtitle in Green"
      )
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
  )
)

# can make some helpers here
#  or add arguments to html_table
#  some examples of styling that we can apply to the table
tbl <- tagAppendAttributes(
  tbl,
  style = "width:80%; margin:0px 50px 0px 50px;"
)

browsable(
  attachDependencies(
    tagList(
      tags$head(
        tags$style(HTML('tr:last-child{border-bottom:2px}'))
      ),
      tbl
    ),
    list(
      rmarkdown:::html_dependency_jquery(),
      bootstrap_dependency()
    )
  )
)
