library(formattable)
library(htmltools)

m_dist <- as.matrix(dist(head(mtcars[,10])))
m_dist[upper.tri(m_dist)] <- NA
m_dist_df <- as.data.frame(m_dist)

ft_html <- format_table(
  m_dist_df,
  formatter = structure(
    lapply(
      1:ncol(m_dist_df),
      function(n){
        formatter(
          "span",
          class = function(x){ifelse(is.na(x), "cell-na-empty", NA)},
          function(x){
            x
          })
      }
    ),
    names = colnames(m_dist_df)
  )
)

ft_html2 <- paste0(
  gsub(
    x = strsplit(ft_html, "\n")[[1]],
    pattern = "<td.*(cell-na-empty).*</td>",
    replacement = "<td style='border:0;'></td>"
  ),
  collapse = "\n"
)

ft<- as.htmlwidget(formattable(data.frame()))
ft$x$html <- ft_html2
ft


# found this https://www.cs.tut.fi/~jkorpela/HTML/emptycells.html
#  &nbsp tricky due to html escape
ft_html3 <- paste0(
  gsub(
    x = strsplit(ft_html, "\n")[[1]],
    pattern = "<td.*(cell-na-empty).*</td>",
    replacement ="<td>&nbsp</td>"
  ),
  collapse = "\n"
)
ft<- as.htmlwidget(formattable(data.frame()))
ft$x$html <- HTML(ft_html3)
ft


# try css empty cells trick
ft_html4 <- paste0(
  gsub(
    x = strsplit(ft_html, "\n")[[1]],
    pattern = "<td.*(cell-na-empty).*</td>",
    replacement = ""
  ),
  collapse = "\n"
)

ft<- as.htmlwidget(formattable(data.frame()))
ft$x$html <- paste0(
  tags$style("table { empty-cells: hide; }"),
  ft_html4,
  collapse="\n"
)
ft





### so it appears the empty-cells: hide
###  is the best solution
###  work through it with longley


(Cl <- data.frame(cor(longley),2))
Cl[upper.tri(Cl)] <- NA

cl_tbl <- format_table(
  table.attr = "style = 'empty-cells:hide;'",
  Cl,
  digits=2
)

browsable(HTML(cl_tbl))

ft <- as.htmlwidget(formattable(data.frame()))
ft$x$html <- paste0(gsub(
  x=strsplit(cl_tbl,"\n")[[1]],
  pattern="(.*<td.*> NA </td>)",
  replacement="",
  perl=TRUE
),collapse="\n")
ft
