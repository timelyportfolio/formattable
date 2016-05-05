# explore DataTables (DT)

# convert formattable to DT
#  https://github.com/renkun-ken/formattable/blob/master/R/formattable.R#L221-L242

# make a custom format_table that just returns the data.frame
#  see https://github.com/renkun-ken/formattable/blob/master/R/formattable.R#L452-L471
format_table_dataframe <- function(
  x, formatters = list(),
  format = c("markdown", "pandoc"), align = "r", ...,
  row.names = rownames(x), check.rows = FALSE, check.names = FALSE
){
  stopifnot(is.data.frame(x))
  if (nrow(x) == 0L) formatters <- list()
  format <- match.arg(format)
  xdf <- data.frame(mapply(function(name, value) {
    f <- formatters[[name]]
    fvalue <- if (is.null(f)) value
    else if (inherits(f, "formula")) formattable:::eval_formula(f, value, x)
    else if (inherits(f, "formatter")) f(value, x)
    else match.fun(f)(value)
    if (is.formattable(fvalue)) as.character.formattable(fvalue) else fvalue
  }, names(x), x, SIMPLIFY = FALSE),
  row.names = row.names,
  check.rows = check.rows,
  check.names = check.names,
  stringsAsFactors = FALSE)
  xdf
}

as.htmldf.formattable <- function(x, format=NULL){
  attrs <- attr(x, "formattable", exact = TRUE)
  if (is.null(attrs)) return(NextMethod("format"))
  format_args <- attrs$format
  format_args[names(format)] <- format
  value <- formattable:::remove_class(x, "formattable")
  if (length(attrs$preproc)) {
    preproc_list <- if (is.list(attrs$preproc)) attrs$preproc else list(attrs$preproc)
    for (preproc in preproc_list) value <- call_or_default(preproc, value)
  }
  # use our custom formatter format_table_dataframe
  str <- do.call("format_table_dataframe", c(list(value), format_args))

  if (x_atomic <- is.atomic(x)) str <- formattable:::remove_attribute(str, "formattable")
  if (length(attrs$postproc)) {
    postproc_list <- if (is.list(attrs$postproc)) attrs$postproc else list(attrs$postproc)
    for (postproc in postproc_list) str <- call_or_default(postproc, str, value)
  }
  str
}

as.datatable.formattable <- function(x, escape=FALSE, ...){
  stopifnot(
    require(DT),
    inherits(x,"formattable")
  )
  datatable(
    as.htmldf.formattable(x),
    escape=escape,
    ...
  )
}

