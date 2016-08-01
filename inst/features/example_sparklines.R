library(dplyr)
library(formattable)
library(sparkline)


res <-
  iris %>%
  group_by(Species) %>%
  summarise(N=n(),
            SL_Mean=round(mean(Sepal.Length),3),
            SL_SD=round(sd(Sepal.Length),3),
            SW_Mean=round(mean(Sepal.Width),3),
            SW_SD=round(sd(Sepal.Width),3)) %>%
  mutate("Sepal.Lengths" = as.character(Species))

#using formattable
ft <- formattable(
  res,
  list(
    SL_Mean=color_bar("pink", proportion),
    "Sepal.Lengths"=function(z){
      sapply(
        z,
        function(zz){
          sprintf(
            '<span class="sparkline-bar">%s</span>',
            paste0(
              iris[which(iris$Species == zz),"Sepal.Length"],
              collapse=","
            )
          )
        }
      )
    }
  )
)

library(htmlwidgets)
browsable(
  attachDependencies(
    tagList(
      onRender(
        as.datatable(ft,options=list(dom='t'),rownames=FALSE),
        "function(el,x){$('.sparkline-bar').sparkline('html',{type:'bar'});}"
      )

    ),
    htmlwidgets:::widget_dependencies("sparkline","sparkline")
  )
)


iris %>%
  group_by(Species) %>%
  summarize_all(function(x){paste0(x,collapse=",")}) %>%
  format_table(
    list(
      area(col=2:5) ~ formatter(
        "span",
        class = "sparkline-box",
        x ~ x
      )
    )
  ) %>%
  HTML %>%
  tagList(
    tags$script(sprintf(
"
  $('.sparkline-box').sparkline(
    'html',
    {type:'box',raw:false, chartRangeMin: 0, chartRangeMax: %f, width:150
  })
"
    ,max(iris[,1:4])
    ))
  ) %>%
  attachDependencies(
    htmlwidgets:::widget_dependencies("sparkline","sparkline")[-1]
  ) %>%
  browsable


iris %>%
  group_by(Species) %>%
  summarize_all(list) %>%
  format_table(
    list(
      area(col=2:5) ~ function(x){
        lapply(x,function(xx){
          as.character(as.tags(
            sparkline(xx,type="box",chartRangeMin=0,chartRangeMax=max(unlist((x))))
          ))
        })
      }
    )
  ) %>%
  HTML %>%
  tagList() %>%
  attachDependencies(
    c(
      list(htmlDependency(
        name = "hack.css",
        version = "0.5.0",
        src = c(href="https://npmcdn.com/hack/dist"),
        stylesheet = c("hack.css", "standard.css")
      )),
      htmlwidgets:::widget_dependencies("sparkline","sparkline")
    )
  ) %>%
  browsable



iris %>%
  group_by(Species) %>%
  summarize_all(list) %>%
  formattable(
    formatters = list(
      area(col=2:5) ~ function(x){
        lapply(x,function(xx){
          as.character(as.tags(
            sparkline(xx,type="box",chartRangeMin=0,chartRangeMax=max(unlist((x))))
          ))
        })
      }
    ),
    table.attr="class='table table-condensed table-striped'"
  ) %>%
  formattable:::as.htmlwidget() %>%
  htmlwidgets:::as.tags.htmlwidget() %>%
  tagList() %>%
  attachDependencies(
    htmlwidgets:::widget_dependencies("sparkline","sparkline")
  ) %>%
  browsable


iris %>%
  group_by(Species) %>%
  summarize_all(list) %>%
  formattable(
    formatters = list(
      area(col=2:5) ~ function(x){
        lapply(x,function(xx){
          as.character(as.tags(
            sparkline(xx,type="bar",chartRangeMin=0,chartRangeMax=max(unlist((x))))
          ))
        })
      }
    ),
    table.attr="class='table table-condensed table-striped'"
  ) %>%
  formattable:::as.htmlwidget() %>%
  htmlwidgets:::as.tags.htmlwidget() %>%
  tagList() %>%
  attachDependencies(
    htmlwidgets:::widget_dependencies("sparkline","sparkline")
  ) %>%
  browsable


ft_df <- formattable(
  iris,
  list(area(col=1:4) ~ color_tile("white","pink"))
) %>%
  formattable:::render_html_matrix.formattable()

colnames(ft_df)[1:4] <- mapply(
  function(column,name){
    paste0(
      name,
      as.character(
        as.tags(
          sparkline(column, type="bar", chartRangeMin = 0, chartRangeMax = max(iris[,1:4]))
        )
      ),
      collapse="<br/>"
    )
  },
  iris[,1:4],
  colnames(iris)[1:4]
)

knitr::kable(ft_df, format="html", escape=FALSE) %>%
  HTML() %>%
  tagList() %>%
  attachDependencies(
    htmlwidgets:::widget_dependencies("sparkline","sparkline")
  ) %>%
  browsable()
