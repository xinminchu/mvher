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

# Internal: check pairs data frame has id1 / id2 columns with atomic types.
.check_pairs <- function(pairs) {
  if (!all(c("id1", "id2") %in% names(pairs))) {
    stop("pairs must have columns 'id1' and 'id2'", call. = FALSE)
  }
  if (!is.atomic(pairs$id1) || !is.atomic(pairs$id2)) {
    stop("pairs$id1 and pairs$id2 must be atomic vectors (integer or character)",
         call. = FALSE)
  }
  invisible(NULL)
}


# Internal: validate id uniqueness in d and referential integrity of pairs.
#
# @param d      data.table with the id column.
# @param pairs  data.frame / data.table with id1 / id2 columns.
# @param id_col Name of the id column in d.  Default "id".
.check_ids <- function(d, pairs, id_col = "id") {
  ids <- d[[id_col]]

  # 1. Uniqueness in d
  dup_ids <- unique(ids[duplicated(ids)])
  if (length(dup_ids) > 0L) {
    stop(
      "Column '", id_col, "' in d must be unique: found ",
      length(dup_ids), " duplicate value(s): ",
      paste(head(dup_ids, 5L), collapse = ", "),
      if (length(dup_ids) > 5L)
        paste0(" ... (", length(dup_ids) - 5L, " more)") else "",
      call. = FALSE
    )
  }

  # 2. Referential integrity: every id in pairs must appear in d
  bad <- union(
    setdiff(unique(pairs$id1), ids),
    setdiff(unique(pairs$id2), ids)
  )
  if (length(bad) > 0L) {
    stop(
      "pairs contains ", length(bad),
      " id value(s) not found in d[['", id_col, "']]: ",
      paste(head(bad, 5L), collapse = ", "),
      if (length(bad) > 5L)
        paste0(" ... (", length(bad) - 5L, " more)") else "",
      call. = FALSE
    )
  }

  invisible(NULL)
}


# Internal: validate Fellegi-Sunter break-point vector.
#
# @param fs_breaks Numeric vector of length 3, strictly increasing values
#   in the open interval (0, 1).
.check_fs_breaks <- function(fs_breaks) {
  if (!is.numeric(fs_breaks) || length(fs_breaks) != 3L) {
    stop("fs_breaks must be a numeric vector of length 3", call. = FALSE)
  }
  if (anyNA(fs_breaks) || any(fs_breaks <= 0) || any(fs_breaks >= 1)) {
    stop("all fs_breaks values must lie in the open interval (0, 1)",
         call. = FALSE)
  }
  if (any(diff(fs_breaks) <= 0)) {
    stop("fs_breaks must be strictly increasing", call. = FALSE)
  }
  invisible(NULL)
}
