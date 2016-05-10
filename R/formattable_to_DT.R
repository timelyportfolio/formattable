#' Convert a \code{link{formattable}} into a \code{data.frame}
#'
#' @param x \code{link{formattable}} object
#' @param formatters named \code{list} of \code{\link{formatters}} to
#'          apply to each column.  These likely have already been specified
#'          and available in the \code{attributes} of the
#'          \code{link{formattable}} object.  If already provided, then this
#'          argument is not necessary.
#' @param align \code{string} "r". I believe this
#'          is not necessary and only a remnant of copy/paste, but I
#'          need to test further.
#' @param row.names see \code{\link{data.frame}}
#' @param check.rows see \code{\link{data.frame}}
#' @param check.names see \code{\link{data.frame}}
format_table_dataframe <- function(
  x, formatters = list(),
  align = "r",
  row.names = rownames(x), check.rows = FALSE, check.names = FALSE
){
  stopifnot(is.data.frame(x))
  if (nrow(x) == 0L) formatters <- list()

  xdf <- data.frame(
    mapply(
      function(name, value) {
        f <- formatters[[name]]
        fvalue <- if (is.null(f)) value
        else if (inherits(f, "formula")) formattable:::eval_formula(f, value, x)
        else if (inherits(f, "formatter")) f(value, x)
        else match.fun(f)(value)
        if (is.formattable(fvalue)) as.character.formattable(fvalue) else fvalue
      },
      names(x),
      x,
      SIMPLIFY = FALSE
    ),
    row.names = row.names,
    check.rows = check.rows,
    check.names = check.names,
    stringsAsFactors = FALSE
  )
  xdf
}


#' Generic function to convert a \code{\link{formattable}}
#'   object into a \code{data.frame} with cells on which
#'   \code{formatters} have been applied.
#'
#' @param x an object
#' @param ... additional arguments
#' @return \code{data.frame}
#' @export
as.htmldf <- function(x, ...) {
  UseMethod("as.htmldf")
}


#' Convert a \code{\link{formattable}} object into a \code{data.frame}
#'  on which \code{formatters} have been applied.  This is an intermediate
#'  step in conversion of a \code{\link{formattable}} into other forms
#'  such as a \code{\link[DT]{datatable}} htmlwidget.
#'
#' @param x \code{\link{formattable}} object
#' @param format_function \code{string} name of a function to process
#'          and format.  \code{}
#'
#' @return \code{\link{data.frame}}
#' @export
as.htmldf.formattable <- function(x, format_function="format_table_dataframe"){
  format <- NULL
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
  htmldf <- do.call(format_function, c(list(value), format_args))

  if (x_atomic <- is.atomic(x)) htmldf <- formattable:::remove_attribute(htmldf, "formattable")
  if (length(attrs$postproc)) {
    postproc_list <- if (is.list(attrs$postproc)) attrs$postproc else list(attrs$postproc)
    for (postproc in postproc_list) htmldf <- call_or_default(postproc, htmldf, value)
  }
  htmldf
}


#' Generic function to create a datatable htmlwidget
#'
#' This function is a generic function to create an \code{htmlwidget}
#' to allow HTML/JS from R in multiple contexts.
#'
#' @param x an object.
#' @param ... arguments to be passed to \code{\link[DT]{datatable}}
#' @export
#' @return a \code{\link[DT]{datatable}} object
as.datatable <- function(x, ...) {
  UseMethod("as.datatable")
}

#' Convert \code{formattable} to a \code{\link[DT]{datatable}} htmlwidget
#'
#' @param x a \code{formattable} object to convert
#' @param escape \code{logical} to escape \code{HTML}.  The default is
#'          \code{FALSE} since it is expected that \code{formatters} from
#'          \code{formattable} will produce \code{HTML} tags.
#' @param ... additional arguments passed to to \code{\link[DT]{datatable}}
#' @return a \code{\link[DT]{datatable}} object
#' @export
as.datatable.formattable <- function(x, escape=FALSE, ...){
  stopifnot(
    require(DT),
    inherits(x,"formattable")
  )
  DT::datatable(
    as.htmldf.formattable(x),
    escape=escape,
    ...
  )
}

