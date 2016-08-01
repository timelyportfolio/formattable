#http://stackoverflow.com/questions/32841221/add-sparkline-graph-to-a-table

library(tables)
library(pipeR)
library(htmltools)

# override print.html from Hmisc with htmltools:::print.html
print.html <- htmltools:::print.html

# print a tables::tabular with htmltools
ht <- tabular(
  (Species + 1) ~ (n=1) + Format(digits=2) *
    (Sepal.Length + Sepal.Width) * (mean + sd),
  data = iris
) %>>%
  (
    capture.output(tables:::html.tabular(.))
  ) %>>%
  HTML

browsable(ht)

# add sparkline dependencies to our ht
ht_dep <- attachDependencies(
  tagList(ht),
  htmlwidgets:::widget_dependencies("sparkline","sparkline")
)

# add script to produce sparklines
ht_dep[[length(ht_dep)+1]] <- tags$script(
"
// add column header for the sparkline
$('table thead tr:last-of-type').append(document.createElement('th'));
$('table thead tr:last-of-type th:last-of-type').text('Mean Length');

var rows = $('table tbody tr').append(document.createElement('td'));
// initialize array for mean lengths
var mean_length = [];
// fill mean length array with the mean length
$('table tbody tr td:nth-of-type(2)').map(function(){
  mean_length.push(parseFloat($(this).text()));
});
// make the sparkline bullet with mean lengths
rows.find('td:last-of-type').each(function(i){
debugger;
  $(this).sparkline(
    //[Math.max.apply(null,mean_length),mean_length[i]],
    [mean_length[mean_length.length-1],mean_length[i],mean_length[i],Math.max.apply(null,mean_length)],
    {type:'bullet', disableTooltips:true}
  )
});
"
)


browsable(ht_dep)


#  plug in output to DataTables
browsable(
  attachDependencies(
    tagList(
      ht,
      tags$script(
"
$('table').dataTable();
"
      )
    ),
    list(
      rmarkdown:::html_dependency_jquery(),
      DT:::DTDependency(style="default")
    )
  )
)
