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
