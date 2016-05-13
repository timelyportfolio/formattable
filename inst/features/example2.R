bootstrap_dependency <- function(){
  htmltools::htmlDependency(
    name = "bootstrap",
    version = "3.3.6",
    src = c(href="http://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/"),
    script = "js/bootstrap.min.js",
    stylesheet = "css/bootstrap.min.css"
  )
}

library("dplyr")
library("broom")
library("stringr")
library("knitr")
library("magrittr")
library("formattable")
library("htmltools")
library("scales")
library("purrr")

fix_names <- . %>%
  str_replace(".Intercept.", "Intercept") %>%
  str_replace("Species", "") %>%
  # Capitalize species names
  str_replace("setosa", "Setosa") %>%
  str_replace("versicolor", "Versicolor") %>%
  str_replace("virginica", "Virginica") %>%
  # Clean up special characters
  str_replace_all(".Width", " Width") %>%
  str_replace_all(".Length", " Length") %>%
  str_replace_all(":", " x ")

# Print with n digits of precision
fixed_digits <- function(xs, n = 2) {
  formatC(xs, digits = n, format = "f")
}

# Don't print leading zero on bounded numbers.
remove_leading_zero <- function(xs) {
  # Problem if any value is greater than 1.0
  digit_matters <- xs %>% as.numeric %>%
    abs %>% is_greater_than(1)
  if (any(digit_matters)) {
    warning("Non-zero leading digit")
  }
  str_replace(xs, "^(-?)0", "\\1")
}

df <- lm(Sepal.Length ~ Species * Sepal.Width, iris) %>%
  tidy %>%
  set_colnames( c("Param", "Estimate", "SE", "_t_", "_p_") ) %>%
  mutate( Param = fix_names( Param ) )

# now do a boxplot sparkline by adding the column
#  and then render with JavaScript
df$Error <- by_row(
  df,
  function(row){
    paste0(
      c(
        row$Estimate - (1.96 * row$SE),
        row$Estimate - (1 * row$SE),
        row$Estimate,
        row$Estimate + (1 * row$SE),
        row$Estimate + (1.96 * row$SE)
      ),
      collapse=","
    )
  },
  .collate="rows"
)$.out

ft_orig <- formattable(
  df,
  list(
    "Estimate" = formatter(
      "div"
      ,style = x ~ mapply(
        function(x,y){
          bgcolor = NA
          if( y >= 5 ) bgcolor = RColorBrewer::brewer.pal(9,"Blues")[9]
          if( y > 1.96 && y < 5 ) bgcolor = colour_ramp(RColorBrewer::brewer.pal(9,"Blues"))(rescale(y,from=c(1.96,5)))
          if( y < -1.96 && y > -5 ) bgcolor = colour_ramp(RColorBrewer::brewer.pal(9,"Reds"))(rescale(y,from=c(-1.96,-5)))
          if( y <= -5 ) bgcolor = RColorBrewer::brewer.pal(9,"Reds")[9]
          style( width = "100%", background = bgcolor )
        },
        x,
        # conditional on t
        as.vector(df[,"_t_"])
      )
      ,"align" = "right"
      ,x ~ digits(x,2)
    ),
    "SE" = function(x){digits(x,2)},
    "_t_" = function(x){digits(x,2)},
    "_p_" = formatter(
      "span"
      ,style = x ~ sapply(
        x,
        function(x){
          if (x>0 && x<=0.05) return(style( color = "blue", font.weight = "bold" ))
          if (x<0 && x>=-0.05) return(style( color = "red", font.weight = "bold" ))
          return( NA )
        }
      )
      ,ps ~ {
        #tiny <- "< .001"
        ps_chr <- ps %>% fixed_digits(3) %>%
          remove_leading_zero
        #ps_chr[ps < 0.001] <- tiny
        ps_chr
      }
    ),
    "Error" = formatter(
      "span",
      class = "sparkline-box",
      x ~ x
    )
  )
)

ft_htmldf <- as.htmldf(ft_orig)


# do as much as possible in Example 2 request in email
browsable(attachDependencies(
  tagList(
    as.datatable(
      ft_orig,
      caption = tagList(
        tags$caption(
          style = "caption-side:top;text-align:left;",
          tags$h3(
            "Linear Regression of Sepal.Length on Species * Sepal.Width"
          )
        ),
        tags$caption(
          style = "caption-side:bottom;font-size:75%;text-align:left;font-style:italic;",
          "Linear Regression; n = 267 cases used in estimation of a total sample size of 896; Cases containing missing values have been excluded; R-Squared: 0.5078; Correct predictions: 52.43%; AIC: 549.09; Results highlighted when p < 0.05"
        )
      ),
      rownames = FALSE,
      colnames = c(
        "Parameter",
        "Estimate",
        "Robust<br/>SE",
        "t value",
        "Pr(>|t|)"
      ),
      class = "table table-striped",
      style = "bootstrap",
      options = list(
        dom = "t",
        fnDrawCallback = htmlwidgets::JS(
"
function(settings){
  $('.sparkline-box:not(:has(canvas))').sparkline(
    'html',
    {type:'box',raw:true,showOutliers:false,disableTooltips:true}
  );
}
"
        )
      )
    )
  ),
  list(
    rmarkdown:::html_dependency_jquery(),
    bootstrap_dependency(),
    htmlwidgets:::getDependency("sparkline")[[3]]
  )
))
