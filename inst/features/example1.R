# example 1
# add titles and captions
# support multiline headings

library(formattable)
library(htmltools)
library(pipeR)
source("./inst/features/formattable_to_DT.R")
# if not cloned locally, source from github
# source("https://rawgit.com/timelyportfolio/formattable/experiments/inst/features/formattable_to_DT.R")


# grades example from http://github.com/renkun-ken/formattable#
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
(
  ft <- formattable(df, list(
    age = color_tile("white", "orange"),
    grade = formatter("span",
                      style = x ~ ifelse(x == "A", style(color = "green", font.weight = "bold"), NA)),
    test1_score = normalize_bar("pink", 0.2),
    test2_score = normalize_bar("pink", 0.2),
    final_score = formatter("span",
                            style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray")),
                            x ~ sprintf("%.2f (rank: %02d)", x, rank(-x))),
    registered = formatter("span",
                           style = x ~ style(color = ifelse(x, "green", "red")),
                           x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No")))
  ))
)


#  using our new helper functions
ft_htmldf <- as.htmldf.formattable(ft)
#  change the column headings
colnames(ft_htmldf) <- c(
  "Unique Identitifier",
  "First Name",
  "Age",
  "Grade",
  "Test Score: 1",
  "Test Score: 2",
  "Final Score",
  "Registration Status"
)

#  do without DT
#   but use our htmldf instead of data.frame
ft_htmldf_ft <- as.htmlwidget(formattable(ft_htmldf))

#  manually build titles and captions
#   this is manual and needs to occur on the JavaScript side
#   will definitely need to clean this up
#   supply arguments/options for captions in formattable?
#   or provide add-ons pipe functions to do what we do below?
library(htmlwidgets)
onRender(
  ft_htmldf_ft,
"
function(el,x){
  // there is a createCaption method
  //  that adds a caption between table and th
  debugger;
  var tbl = $(el).find('table')[0];

  // add the title
  var title = tbl.createCaption();
  $(title).css({
    'caption-side':'top',
    'text-align':'center'
  });
  title.innerHTML = '<h3>Test Results</h3><h4>Class B</h4>'

  // add the footer caption
  $(tbl).append('<caption></caption>');
  var caption = $(tbl).find('caption:nth-of-type(2)');
  caption.css({
    'text-align':'left',
    'font-style':'italic',
    'font-weight':'bold',
    'font-size':'75%',
    'caption-side':'bottom'
  });
  caption.text(
    [
      'The data shown is from a standarized inventory',
      'testing scheme implemented as a result of',
      'Government Regulation 142.  Questionnaires',
      'were administered July 21.  Students with',
      'learning difficulties received coaching during testing.'
    ].join(' ')
  );

  // demonstrate changing width of a column
  $($(tbl).find('th')[0]).css({
    'width': '50px',
    'word-break': 'break-word'
  });
}
"
)

onRender(
  as.datatable.formattable(
    formattable(ft_htmldf),
    options=list(dom='t')
  ),
  "
  function(el,x){
  // there is a createCaption method
  //  that adds a caption between table and th
  debugger;
  var tbl = $(el).find('table')[0];

  // add the title
  var title = tbl.createCaption();
  $(title).css({
  'caption-side':'top',
  'text-align':'center'
  });
  title.innerHTML = '<h3>Test Results</h3><h4>Class B</h4>'

  // add the footer caption
  $(tbl).append('<caption></caption>');
  var caption = $(tbl).find('caption:nth-of-type(2)');
  caption.css({
  'text-align':'left',
  'font-style':'italic',
  'font-weight':'bold',
  'font-size':'75%',
  'caption-side':'bottom'
  });
  caption.text(
  [
  'The data shown is from a standarized inventory',
  'testing scheme implemented as a result of',
  'Government Regulation 142.  Questionnaires',
  'were administered July 21.  Students with',
  'learning difficulties received coaching during testing.'
  ].join(' ')
  );

  // demonstrate changing width of a column
  $($(tbl).find('th')[0]).css({
  'width': '50px',
  'word-break': 'break-word'
  });
  }
  "
)
