library(formattable)

# example from Readme
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
  stringsAsFactors = FALSE
)

# make a formattable for testing
fdf <- formattable(df, list(
  age = color_tile("white", "orange"),
  grade = formatter("span",
                    style = x ~ ifelse(x == "A", style(color = "green", font.weight = "bold"), NA)),
  test1_score = proportion_bar("pink", 0.2),
  test2_score = normalize_bar("pink", 0.2),
  final_score = formatter("span",
                          style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray")),
                          x ~ sprintf("%.2f (rank: %02d)", x, rank(-x))),
  registered = formatter("span",
                         style = x ~ style(color = ifelse(x, "green", "red")),
                         x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No")))
))

fdf

# change the data.frame column headings with space or newline
#  doing with colnames will require multiple steps
#  but let's see how it looks
# it will break the join/key though for the defined formats
#  stored in attributes for formattable
fdf2<-fdf
colnames(fdf2)[5] <- "test1<br/>score"
fdf2


# explore hyphenate
#devtools::install_github("hrbrmstr/hyphenatr")


