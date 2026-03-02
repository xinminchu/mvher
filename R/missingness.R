#' Summarise field-level missingness in a record dataset
#'
#' Counts empty strings and \code{NA}s per text column and reports the
#' fraction missing.  Useful as a first-pass diagnostic before choosing a
#' missing-value handling strategy.
#'
#' @param d A \code{data.frame} or \code{data.table} of records.
#' @param text_cols Character vector of column names to inspect.
#'
#' @return A \code{data.table} with columns:
#'   \describe{
#'     \item{\code{column}}{Column name.}
#'     \item{\code{n_total}}{Total number of rows.}
#'     \item{\code{n_empty}}{Number of rows where the field is \code{NA} or \code{""}.}
#'     \item{\code{pct_miss}}{Percentage missing (0–100), rounded to 2 d.p.}
#'   }
#'
#' @importFrom data.table data.table rbindlist
#' @export
#' @examples
#' d <- data.frame(id = 1:4,
#'                 name = c("Alice", "", NA, "Bob"),
#'                 addr = c("", "", "X", "Y"))
#' mv_field_summary(d, c("name", "addr"))
mv_field_summary <- function(d, text_cols) {
  .check_text_cols(d, text_cols)
  out <- lapply(text_cols, function(col) {
    vals    <- d[[col]]
    n_total <- length(vals)
    n_empty <- sum(is.na(vals) | vals == "")
    data.table::data.table(
      column   = col,
      n_total  = n_total,
      n_empty  = n_empty,
      pct_miss = round(100 * n_empty / max(n_total, 1L), 2)
    )
  })
  data.table::rbindlist(out)
}


#' Summarise missingness at the pair level
#'
#' For each pair \code{(id1, id2)} and each text column, reports how many
#' pairs have at least one empty field (\code{"any_missing"}), both empty
#' (\code{"both_missing"}), or exactly one empty (\code{"one_missing"}).
#'
#' @param d Record dataset.  Must have an \code{id} column and all columns in
#'   \code{text_cols}.
#' @param pairs \code{data.frame} or \code{data.table} with columns
#'   \code{id1} and \code{id2}.
#' @param text_cols Character vector of column names.
#'
#' @return A \code{data.table} with columns \code{column}, \code{type},
#'   \code{n_pairs}, \code{pct_pairs}.
#'
#' @importFrom data.table as.data.table data.table rbindlist setkey
#' @export
#' @examples
#' d <- data.frame(id = 1:4, name = c("Alice", "", NA, "Bob"))
#' pairs <- data.frame(id1 = c(1, 2, 3), id2 = c(2, 3, 4))
#' mv_pair_summary(d, pairs, "name")
mv_pair_summary <- function(d, pairs, text_cols) {
  d <- data.table::as.data.table(d)
  p <- data.table::as.data.table(pairs)
  .check_text_cols(d, text_cols)
  .check_pairs(p)

  if (!"id" %in% names(d)) stop("d must have an 'id' column", call. = FALSE)
  data.table::setkey(d, id)

  a <- d[.(p$id1)]
  b <- d[.(p$id2)]
  n_pairs <- nrow(p)

  out <- lapply(text_cols, function(col) {
    xa <- a[[col]]; xb <- b[[col]]
    miss_a <- is.na(xa) | xa == ""
    miss_b <- is.na(xb) | xb == ""
    n_any  <- sum(miss_a | miss_b)
    n_both <- sum(miss_a & miss_b)
    n_one  <- n_any - n_both
    data.table::data.table(
      column    = col,
      type      = c("any_missing", "both_missing", "one_missing"),
      n_pairs   = c(n_any, n_both, n_one),
      pct_pairs = round(100 * c(n_any, n_both, n_one) / max(n_pairs, 1L), 2)
    )
  })
  data.table::rbindlist(out)
}


#' Estimate the missingness mechanism (MCAR / MAR / MNAR heuristic)
#'
#' Tests for association between missingness and entity membership using a
#' chi-squared test of independence.  High association suggests MAR or MNAR;
#' low association is consistent with MCAR.
#'
#' @param d Record dataset.  Must contain \code{entity_col} and all columns in
#'   \code{text_cols}.
#' @param text_cols Character vector of column names to test.
#' @param entity_col Name of the column identifying entities.
#'   Default \code{"entity_id"}.
#' @param alpha Significance level for the chi-squared test.  Default \code{0.05}.
#'
#' @return A \code{data.table} with one row per column and columns:
#'   \describe{
#'     \item{\code{column}}{Column name.}
#'     \item{\code{overall_miss_rate}}{Fraction of rows that are missing.}
#'     \item{\code{entity_miss_variance}}{Variance of per-entity missingness rates.}
#'     \item{\code{chi_sq_p}}{p-value from chi-squared test (simulated).}
#'     \item{\code{mechanism}}{Either \code{"MCAR"} or \code{"MAR/MNAR"}.}
#'   }
#'
#' @details
#' This is a heuristic only.  A significant chi-squared p-value indicates that
#' missingness is associated with entity identity, which is consistent with MAR
#' (missing at random given entity) or MNAR.  It cannot distinguish MAR from
#' MNAR without external covariates.
#'
#' @importFrom data.table data.table rbindlist as.data.table
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   entity_id = rep(1:5, each = 4),
#'   name = ifelse(runif(20) < 0.3, NA_character_, paste0("name", 1:20))
#' )
#' mv_detect_mechanism(d, "name")
mv_detect_mechanism <- function(d, text_cols, entity_col = "entity_id", alpha = 0.05) {
  d <- data.table::as.data.table(d)
  .check_text_cols(d, text_cols)
  if (!entity_col %in% names(d)) {
    stop("entity_col '", entity_col, "' not found in data", call. = FALSE)
  }

  out <- lapply(text_cols, function(col) {
    is_miss <- as.integer(is.na(d[[col]]) | d[[col]] == "")
    entity  <- d[[entity_col]]

    # Chi-squared test: missingness ~ entity
    tab   <- table(entity, is_miss)
    p_val <- NA_real_
    if (ncol(tab) == 2 && nrow(tab) >= 2) {
      ct <- tryCatch(
        suppressWarnings(chisq.test(tab, simulate.p.value = TRUE, B = 999)),
        error = function(e) NULL
      )
      if (!is.null(ct)) p_val <- ct$p.value
    }

    ent_rates <- tapply(is_miss, entity, mean, na.rm = TRUE)
    variance  <- if (length(ent_rates) > 1) var(ent_rates, na.rm = TRUE) else 0
    mechanism <- if (is.na(p_val)) "unknown" else if (p_val < alpha) "MAR/MNAR" else "MCAR"

    data.table::data.table(
      column            = col,
      overall_miss_rate = round(mean(is_miss, na.rm = TRUE), 4),
      entity_miss_variance = round(variance, 6),
      chi_sq_p          = round(p_val, 4),
      mechanism         = mechanism
    )
  })
  data.table::rbindlist(out)
}


#' Measure entity-correlated missingness via one-way ICC
#'
#' Computes the intra-class correlation (ICC) of the per-record missingness
#' indicator with respect to entity membership.  High ICC means entire entities
#' tend to be missing together (systematic / MNAR pattern), which creates
#' covariate shift under entity-disjoint train/test splits.
#'
#' @param d Record dataset with an entity membership column.
#' @param text_cols Character vector of column names.
#' @param entity_col Name of the entity column.  Default \code{"entity_id"}.
#'
#' @return A \code{data.table} with columns \code{column}, \code{icc} (0–1),
#'   and \code{interpretation} (\code{"low (MCAR-like)"},
#'   \code{"moderate (MAR-like)"}, or \code{"high (MNAR-like)"}).
#'
#' @details
#' ICC is estimated using the one-way random-effects ANOVA formula:
#' \deqn{ICC = \frac{MS_B - MS_W}{MS_B + (n_0 - 1) MS_W}}
#' where \eqn{n_0} is the harmonic-like mean group size.  Values below 0.1
#' suggest MCAR; values above 0.4 suggest systematic entity-level absence.
#'
#' @importFrom data.table data.table rbindlist as.data.table
#' @export
#' @examples
#' set.seed(2)
#' d <- data.frame(
#'   entity_id = rep(1:10, each = 5),
#'   name = c(rep(NA_character_, 10), paste0("n", 11:50))
#' )
#' mv_entity_correlation(d, "name")
mv_entity_correlation <- function(d, text_cols, entity_col = "entity_id") {
  d <- data.table::as.data.table(d)
  .check_text_cols(d, text_cols)
  if (!entity_col %in% names(d)) {
    stop("entity_col '", entity_col, "' not found in data", call. = FALSE)
  }

  out <- lapply(text_cols, function(col) {
    is_miss    <- as.numeric(is.na(d[[col]]) | d[[col]] == "")
    entity     <- d[[entity_col]]
    grand_mean <- mean(is_miss, na.rm = TRUE)
    ent_means  <- tapply(is_miss, entity, mean, na.rm = TRUE)
    ent_ns     <- as.numeric(tapply(is_miss, entity, length))
    n_total    <- length(is_miss)
    k          <- length(ent_means)

    if (k <= 1) {
      return(data.table::data.table(
        column         = col,
        icc            = NA_real_,
        interpretation = "insufficient groups"
      ))
    }

    ss_between <- sum(ent_ns * (ent_means - grand_mean)^2, na.rm = TRUE)
    ss_within  <- sum(vapply(unique(entity), function(e) {
      vals <- is_miss[entity == e]
      sum((vals - mean(vals, na.rm = TRUE))^2, na.rm = TRUE)
    }, numeric(1)), na.rm = TRUE)

    ms_between <- ss_between / (k - 1)
    n0         <- (n_total - sum(ent_ns^2) / n_total) / (k - 1)
    ms_within  <- if (n_total - k > 0) ss_within / (n_total - k) else 0

    denom  <- ms_between + (n0 - 1) * ms_within
    icc_val <- if (abs(denom) < 1e-12) 0 else (ms_between - ms_within) / denom
    icc_val <- max(0, min(1, icc_val))

    interp <- if (icc_val < 0.1) "low (MCAR-like)" else
              if (icc_val < 0.4) "moderate (MAR-like)" else "high (MNAR-like)"

    data.table::data.table(
      column         = col,
      icc            = round(icc_val, 4),
      interpretation = interp
    )
  })
  data.table::rbindlist(out)
}
