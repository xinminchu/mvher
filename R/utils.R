#' Null-coalescing operator
#'
#' Returns \code{x} if not \code{NULL}, otherwise \code{y}.
#'
#' @param x Object to test.
#' @param y Default value returned when \code{x} is \code{NULL}.
#' @return \code{x} if non-\code{NULL}, else \code{y}.
#' @keywords internal
`%||%` <- function(x, y) if (!is.null(x)) x else y


#' Normalise a character vector for ER feature extraction
#'
#' Converts to lower-case, collapses internal whitespace, strips leading/trailing
#' whitespace, and replaces \code{NA} with \code{""}.  This is the standard
#' pre-processing step applied to text columns before pairwise comparison.
#'
#' @param x Character vector (or coercible to character).
#' @return A character vector of the same length with no \code{NA}s.
#' @export
#' @examples
#' norm_str(c("  Hello World ", NA, "foo  bar"))
#' # [1] "hello world" ""          "foo bar"
norm_str <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  tolower(x)
}


# Internal: validate text_cols against a data frame / data table.
.check_text_cols <- function(d, text_cols) {
  miss <- setdiff(text_cols, names(d))
  if (length(miss) > 0) {
    stop("text_cols not found in data: ", paste(miss, collapse = ", "), call. = FALSE)
  }
  invisible(NULL)
}

# Internal: check pairs data frame has id1 / id2 columns.
.check_pairs <- function(pairs) {
  if (!all(c("id1", "id2") %in% names(pairs))) {
    stop("pairs must have columns 'id1' and 'id2'", call. = FALSE)
  }
  invisible(NULL)
}
