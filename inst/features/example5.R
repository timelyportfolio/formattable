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
