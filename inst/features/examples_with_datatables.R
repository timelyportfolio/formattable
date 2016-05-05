# experiment with formattable as DT

library(formattable)
library(purrr)
source("./inst/features/formattable_to_DT.R")

# helper function to compare formattable htmlwidget with datatable render
library(htmltools)
compare_widgets <- function(fdf){
  browsable(
    tagList(
      formattable:::as.htmlwidget.formattable(fdf),
      as.datatable.formattable(fdf, style="bootstrap")
    )
  )
}

# ? formattable.data.frame
examples <- list(
  # mtcars (mpg in red)
  formattable(mtcars,
            list(mpg = formatter("span", style = "color:red")))


  # mtcars (mpg in red if greater than median)
  ,formattable(mtcars, list(mpg = formatter("span",
                                           style = function(x) ifelse(x > median(x), "color:red", NA))))

  # mtcars (mpg in red if greater than median, using formula)
  ,formattable(mtcars, list(mpg = formatter("span",
                                           style = x ~ ifelse(x > median(x), "color:red", NA))))

  # mtcars (mpg in gradient: the higher, the redder)
  ,formattable(mtcars, list(mpg = formatter("span",
                                           style = x ~ style(color = rgb(x/max(x), 0, 0)))))

  # mtcars (mpg background in gradient: the higher, the redder)
  ,formattable(mtcars, list(mpg = formatter("span",
                                           style = x ~ style(display = "block",
                                                             "border-radius" = "4px",
                                                             "padding-right" = "4px",
                                                             color = "white",
                                                             "background-color" = rgb(x/max(x), 0, 0)))))

  # mtcars (mpg in red if vs == 1 and am == 1)
  ,formattable(mtcars, list(mpg = formatter("span",
                                           style = ~ style(color = ifelse(vs == 1 & am == 1, "red", NA)))))
)

map(examples,~compare_widgets(head(.x,10)))








examples_rmd <- list()
# demo 1

examples_rmd[[length(examples_rmd)+1]] <- formattable(mtcars, list(
  mpg = formatter("span", style = function(x)
    style(color = ifelse(x > quantile(x, 0.8), "red", NA))),
  am = formatter("span", function(x) ifelse(x == 1, "yes", "no")),
  gear = formatter("span", style = function(x)
    style(font.weight = ifelse(x == 4, "bold", NA))),
  qsec = formatter("span", style = function(x, m = qrank(x))
    style(color = rgb(m, 0, 0)))))


# demo 2

data(CO2)
examples_rmd[[length(examples_rmd)+1]] <- formattable(CO2, list(
  conc = formatter("span", style = function(x, m = 1 - x/max(x) * 0.8, ms = round(1-m))
    style(display = "block", border.radius = "4px", background.color = rgb(0, m, 0),
          padding.right = "4px", color = rgb(1,1,1))),
  uptake = formatter("span", style = function(x, m = 1-x/max(x), ms = round(1-m))
    style(display = "block", border.radius = "4px", padding.right = "4px",
          background.color = rgb(m, m, 0), color = rgb(ms,ms,ms)))))


# demo 3

df <- data.frame(
  id = 1:10,
  name = c("Bob", "Ashley", "James", "David", "Jenny",
           "Hans", "Leo", "John", "Emily", "Lee"),
  age = c(28, 27, 30, 28, 29, 29, 27, 27, 31, 30),
  grade = c("C", "A", "A", "C", "B", "B", "B", "A", "C", "C"),
  test1_score = c(8.9, 9.5, 9.6, 8.9, 9.1, 9.3, 9.3, 9.9, 8.5, 8.6),
  test2_score = c(9.1, 9.1, 9.2, 9.1, 8.9, 8.5, 9.2, 9.3, 9.1, 8.8),
  final_score = c(9, 9.3, 9.4, 9, 9, 8.9, 9.25, 9.6, 8.8, 8.7),
  registered = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE)

score_colorizer <- formatter("span",
                             style = function(x) style(
                               display = "block",
                               color = "white",
                               border.radius = "4px",
                               padding.right = "4px",
                               background = rgb(0.2 + 0.8 * qrank(-x), 0.6, 0)))

examples_rmd[[length(examples_rmd)+1]] <- formattable(df, list(
  grade = formatter("span",
                    style = function(x)
                      ifelse(x == "A", style(
                        color = "green",
                        font.weight = "bold"), NA)),
  test1_score = score_colorizer,
  test2_score = score_colorizer,
  final_score = formatter("span",
                          style = function(x) style(
                            color = ifelse(rank(-x) <= 3, "green", "gray")),
                          function(x) sprintf("%.2f (rank: %02d)", x, rank(-x))),
  registered = function(x) ifelse(x, "yes", "no")
))


# demo 4
examples_rmd[[length(examples_rmd)+1]] <- formattable(df, list(
  # bootstrap glyphicons
  grade = formatter("span",
                    style = x ~ style(color = vmap(x, A="darkgreen",B="orange",C="darkred")),
                    x ~ icontext(vmap(x, A="star",B="ok",C="remove"), x)),
  # bar
  test1_score = formatter("span",
                          style = x ~ style(color = "white", background = "green",
                                            padding.left = sprintf("%.0fpx", 4 + 76 * normalize(x)),
                                            padding.right = "4px",
                                            border.radius = "4px")),
  # bar
  test2_score = formatter("span",
                          style = x ~ style(color = "white", background = "green",
                                            padding.right = sprintf("%.0fpx", 4 + 76 * normalize(x)),
                                            padding.left = "4px",
                                            border.radius = "4px")),
  # gradient
  final_score = formatter("span",
                          style = x ~ style(display = "block",
                                            background = rgb(0.3, 0, 1 - 0.8 * qrank(x)),
                                            color = "white", padding.right = "5px", border.radius = "4px")),
  # text transform
  registered = x ~ ifelse(x, "yes", "no")),
  align = c("r","l","r","c","r","l","r","c"))


# demo 5

examples_rmd[[length(examples_rmd)+1]] <- formattable(df, list(
  grade = formatter("span",
                    style = x ~ vmap(x, A = style(color = "green", font.weight = "bold"), NA)),
  test1_score = formatter("span",
                          style = x ~ style(color = csscolor(gradient(rank(x), "black", "red")))),
  test2_score = formatter("span",
                          style = x ~ style(color = csscolor(gradient(rank(x), "black", "red")))),
  final_score = formatter("span",
                          style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray"))),
  registered = x ~ ifelse(x, "yes", "no")
))


map(examples_rmd,~compare_widgets(head(.x,15)))







library("dplyr")
library("broom")
library("stringr")
library("knitr")

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

lm(Sepal.Length ~ Species * Sepal.Width, iris) %>%
  tidy %>%
  set_colnames( c("Param", "Estimate", "SE", "_t_", "_p_") ) %>%
  mutate( Param = fix_names( Param ) ) %>%
  formattable(
    list(
      "_p_" = formatter(
        "span"
        ,style = x ~ ifelse( x < 0.05, style( color = "red", font.weight = "bold" ), NA )
        ,ps ~ {
          tiny <- "< .001"
          ps_chr <- ps %>% fixed_digits(3) %>%
            remove_leading_zero
          ps_chr[ps < 0.001] <- tiny
          ps_chr
        }
      )
    )
    ,digits=2
  ) %>%
  as.datatable.formattable(options=list(dom='t'), rownames=FALSE) %>%
  formatRound(columns=c("Estimate","SE","_t_"),digits=3)
