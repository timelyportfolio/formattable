# install experiments branch of forked formattable repo
#devtools::install_github("timelyportfolio/formattable@experiments")

library(formattable)

# check to see if new generic methods available
methods(class="formattable")
# there should as.htmldf and as.datatable
#  so we should get TRUE TRUE for this next statement
c("as.datatable.formattable","as.htmldf.formattable") %in% as.character(methods(class="formattable"))


# let's see if these generic methods work for a simple
#  formattable
as.htmldf(formattable(mtcars))
formattable(as.htmldf(formattable(mtcars)))
as.datatable(formattable(mtcars))
