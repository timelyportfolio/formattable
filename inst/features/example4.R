# from @renkun-ken html-table branch
#  https://github.com/renkun-ken/formattable/blob/html-table/R/html_table.R

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
                    tag("th", list(align = align, style = style, HTML(column)))
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
tFormatter <- formatter("span",
                        style = x ~ style(display = "block",
                                          padding = "0 4px", `border-radius` = "4px", `background-color` = csscolor(gradient(abs(x),"white", "orange"))),
                        ~ fixedDigits(t, 2)
)

# Example
coef.matrix <- summary(lm(Sepal.Length ~ Species * Sepal.Width, iris))$coef
coef.df <- as.data.frame(coef.matrix)
rownames(coef.df)[2] = "A label with some spaces"
colnames(coef.df)[2:4] <- c("Standard<br>Error", "t", "p")
ft <- formattable(coef.df,
            list(Estimate = estimateFormatter, "p" = pFormatter, "t" = tFormatter))
ft



# play with the html_table function
library(htmltools)
browsable(
  html_table(
    as.htmldf(ft)
  )
)
