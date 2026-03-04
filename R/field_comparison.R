#' Pairwise field comparison with missing-value handling
#'
#' Computes string similarity between two character vectors \code{xa} and
#' \code{xb} using Jaro-Winkler (JW) and normalised Levenshtein (LV), with
#' configurable treatment of empty/NA fields.
#'
#' @param xa,xb Character vectors of equal length (one entry per candidate pair).
#' @param method Missing-value handling strategy.  One of:
#'   \describe{
#'     \item{\code{"current"}}{Compute similarity as-is.  Both-empty pairs
#'       receive sim = 1 because \code{stringdist("","") = 0}.  Backward-
#'       compatible but semantically wrong for entity resolution.}
#'     \item{\code{"neutral"}}{Set sim = 0.5 when either field is empty.
#'       Corresponds to the \emph{absent} (γ = 0) level in Fellegi-Sunter theory,
#'       which contributes neither evidence for nor against a match.}
#'     \item{\code{"zero"}}{Set sim = 0.0 when either field is empty.
#'       Conservative: penalises pairs with any missing field.}
#'     \item{\code{"indicator"}}{Keep raw similarity; additionally return
#'       binary \code{any_miss} and \code{both_miss} flags.  Use with
#'       \code{\link{mv_pair_features}} to add indicator columns to the feature
#'       matrix.}
#'     \item{\code{"full"}}{Neutral sim (0.5) + indicator columns.  Recommended
#'       when using entity-disjoint train/test splits and entity-correlated
#'       missingness, as indicators allow the model to learn a negative weight
#'       for the both-empty artefact.}
#'     \item{\code{"fellegi_sunter"}}{Returns an integer \eqn{\gamma}-level
#'       (0 = missing, 1--4 = increasing agreement tiers) instead of a
#'       continuous score.  Tier boundaries are set by \code{fs_breaks}.}
#'     \item{\code{"complete_cases"}}{Set sim = \code{NA} for pairs where
#'       either field is empty.  Downstream imputation or complete-case
#'       filtering is then required.}
#'     \item{\code{"asymmetric"}}{Distinguishes \emph{both-empty} (0.0) from
#'       \emph{one-empty} (0.5) from \emph{both-present} (raw sim).  Avoids
#'       the both-null=1 artefact while being less aggressive than
#'       \code{"zero"}.}
#'   }
#' @param jw_p Jaro-Winkler prefix penalty.  Default \code{0.1}.
#' @param fs_breaks Numeric vector of length 3 giving the JW thresholds for
#'   Fellegi-Sunter \eqn{\gamma}-levels low / medium / high / very-high.
#'   Default \code{c(0.25, 0.60, 0.85)}.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{\code{jw}}{Numeric vector of JW similarities (\code{NA} for
#'       \code{complete_cases} on missing pairs; integer for
#'       \code{fellegi_sunter}).}
#'     \item{\code{lv}}{Numeric vector of normalised LV similarities (same
#'       missingness treatment as \code{jw}; \code{NULL} for
#'       \code{fellegi_sunter}).}
#'     \item{\code{any_miss}}{Logical vector: TRUE when either \code{xa} or
#'       \code{xb} is empty/NA.  Always returned.}
#'     \item{\code{both_miss}}{Logical vector: TRUE when both are empty/NA.
#'       Always returned.}
#'     \item{\code{gamma}}{Integer vector of \eqn{\gamma}-levels.  Only
#'       returned for method \code{"fellegi_sunter"}.}
#'   }
#'
#' @importFrom stringdist stringdist
#' @export
#' @examples
#' xa <- c("Alice",  "Bob",  "",      "Carol")
#' xb <- c("Alice B","Bobby","",      NA)
#' mv_compare_fields(xa, xb, method = "full")
#' mv_compare_fields(xa, xb, method = "fellegi_sunter")
mv_compare_fields <- function(xa, xb,
                              method = c("current", "neutral", "zero",
                                         "indicator", "full",
                                         "fellegi_sunter",
                                         "complete_cases",
                                         "asymmetric"),
                              jw_p      = 0.1,
                              fs_breaks = c(0.25, 0.60, 0.85)) {
  method <- match.arg(method)
  if (method == "fellegi_sunter") .check_fs_breaks(fs_breaks)
  xa <- as.character(xa)
  xb <- as.character(xb)
  xa[is.na(xa)] <- ""
  xb[is.na(xb)] <- ""

  miss_a <- xa == ""
  miss_b <- xb == ""
  any_m  <- miss_a | miss_b
  both_m <- miss_a & miss_b

  # Raw JW similarity
  jw_raw <- 1 - stringdist::stringdist(xa, xb, method = "jw", p = jw_p)
  jw_raw <- pmax(0, pmin(1, jw_raw))

  # Raw normalised-LV similarity
  lv_d   <- stringdist::stringdist(xa, xb, method = "lv")
  lv_m   <- pmax(nchar(xa), nchar(xb))
  lv_raw <- 1 - ifelse(lv_m == 0, 0, lv_d / lv_m)
  lv_raw <- pmax(0, pmin(1, lv_raw))

  result <- list(any_miss = any_m, both_miss = both_m)

  if (method == "fellegi_sunter") {
    gamma <- integer(length(jw_raw))
    gamma[any_m]                                              <- 0L
    gamma[!any_m & jw_raw < fs_breaks[1]]                    <- 1L
    gamma[!any_m & jw_raw >= fs_breaks[1] &
            jw_raw < fs_breaks[2]]                           <- 2L
    gamma[!any_m & jw_raw >= fs_breaks[2] &
            jw_raw < fs_breaks[3]]                           <- 3L
    gamma[!any_m & jw_raw >= fs_breaks[3]]                   <- 4L
    result$jw    <- gamma   # reuse slot for the gamma encoding
    result$lv    <- NULL
    result$gamma <- gamma
    return(result)
  }

  jw <- jw_raw
  lv <- lv_raw

  if (method %in% c("neutral", "full")) {
    jw[any_m] <- 0.5
    lv[any_m] <- 0.5
  } else if (method == "zero") {
    jw[any_m] <- 0.0
    lv[any_m] <- 0.0
  } else if (method == "complete_cases") {
    jw[any_m] <- NA_real_
    lv[any_m] <- NA_real_
  } else if (method == "asymmetric") {
    only_one_m     <- any_m & !both_m
    jw[both_m]     <- 0.0
    lv[both_m]     <- 0.0
    jw[only_one_m] <- 0.5
    lv[only_one_m] <- 0.5
  }
  # "current" and "indicator": leave jw/lv as computed

  result$jw <- jw
  result$lv <- lv
  result
}


#' Extract pairwise text features with missing-value handling
#'
#' For each pair \code{(id1, id2)} and each column in \code{text_cols},
#' computes Jaro-Winkler and/or normalised Levenshtein similarity using the
#' chosen \code{method}.  Returns a feature matrix ready for a classifier
#' alongside match labels derived from entity membership.
#'
#' @param d Record dataset (\code{data.frame} or \code{data.table}) that must
#'   have columns \code{id}, \code{entity_id}, and all of \code{text_cols}.
#' @param pairs \code{data.frame}/\code{data.table} with columns \code{id1},
#'   \code{id2}.
#' @param text_cols Character vector of column names to use as features.
#' @param method Missing-value handling strategy passed to
#'   \code{\link{mv_compare_fields}}.  Default \code{"current"}.
#' @param similarity Which similarity metrics to include in the feature matrix:
#'   \code{"both"} (JW + LV, default), \code{"jw"} (JW only), or
#'   \code{"lv"} (LV only).  Ignored for \code{method = "fellegi_sunter"},
#'   which always returns a single \eqn{\gamma} column per text field.
#' @param chunk_size Optional integer.  If supplied and \code{nrow(pairs) >
#'   chunk_size}, computation is split into chunks to reduce peak memory
#'   usage.
#' @param jw_p Jaro-Winkler prefix penalty.  Default \code{0.1}.
#' @param fs_breaks Fellegi-Sunter \eqn{\gamma}-level thresholds.
#'   Default \code{c(0.25, 0.60, 0.85)}.
#'
#' @return A list with:
#'   \describe{
#'     \item{\code{X}}{A \code{data.table} of numeric features (one row per
#'       pair).  Column names follow the pattern \code{<col>_jw},
#'       \code{<col>_lv}, \code{<col>_gamma}, \code{<col>_any_miss},
#'       \code{<col>_both_miss} as appropriate for the chosen \code{method}.}
#'     \item{\code{y}}{Integer match label vector (1 = same entity, 0 =
#'       different entity).}
#'   }
#'
#' @importFrom data.table as.data.table data.table rbindlist setkey
#' @export
#' @examples
#' d <- data.frame(
#'   id        = 1:4,
#'   entity_id = c(1L, 1L, 2L, 2L),
#'   name      = c("Alice", "Alice B.", NA, "Carol")
#' )
#' pairs <- data.frame(id1 = c(1, 1, 3), id2 = c(2, 3, 4))
#' mv_pair_features(d, pairs, text_cols = "name", method = "neutral")
#' mv_pair_features(d, pairs, text_cols = "name", method = "full")
mv_pair_features <- function(d, pairs, text_cols,
                             method = c("current", "neutral", "zero",
                                        "indicator", "full",
                                        "fellegi_sunter",
                                        "complete_cases",
                                        "asymmetric"),
                             similarity = c("both", "jw", "lv"),
                             chunk_size = NULL,
                             jw_p       = 0.1,
                             fs_breaks  = c(0.25, 0.60, 0.85)) {
  method     <- match.arg(method)
  similarity <- match.arg(similarity)

  # --- Copy to avoid setkey() modifying the caller's data.table in-place ---
  d     <- data.table::copy(data.table::as.data.table(d))
  pairs <- data.table::as.data.table(pairs)

  # --- Validate inputs upfront (once, before any chunking) -----------------
  if (!"id" %in% names(d)) {
    stop("d must have an 'id' column", call. = FALSE)
  }
  if (!"entity_id" %in% names(d)) {
    stop("d must have an 'entity_id' column", call. = FALSE)
  }
  .check_text_cols(d, text_cols)
  .check_pairs(pairs)
  if (nrow(pairs) > 0L) .check_ids(d, pairs)
  if (method == "fellegi_sunter") .check_fs_breaks(fs_breaks)

  if (nrow(pairs) == 0L) {
    return(list(X = data.table::data.table(), y = integer()))
  }

  # --- Pre-normalise text columns once (lowercase, trim, NA -> "") ----------
  for (col in text_cols) {
    data.table::set(d, j = col, value = norm_str(d[[col]]))
  }

  # --- Key once; chunked iteration reuses this keyed table -----------------
  data.table::setkey(d, id)

  if (!is.null(chunk_size) && nrow(pairs) > chunk_size) {
    n      <- nrow(pairs)
    chunks <- ceiling(n / chunk_size)
    x_list <- vector("list", chunks)
    y_list <- vector("list", chunks)
    for (i in seq_len(chunks)) {
      i1 <- (i - 1L) * chunk_size + 1L
      i2 <- min(i * chunk_size, n)
      tmp          <- .features_chunk(d, pairs[i1:i2], text_cols,
                                      method, similarity,
                                      jw_p, fs_breaks)
      x_list[[i]] <- tmp$X
      y_list[[i]] <- tmp$y
    }
    return(list(
      X = data.table::rbindlist(x_list, fill = TRUE),
      y = unlist(y_list)
    ))
  }

  .features_chunk(d, pairs, text_cols, method, similarity, jw_p, fs_breaks)
}


# Internal: compute features for one (possibly chunked) slice of pairs.
# Assumes d is already setkey'd on 'id' and text_cols are pre-normalised.
.features_chunk <- function(d, pairs, text_cols,
                            method, similarity, jw_p, fs_breaks) {
  a <- d[.(pairs$id1)]
  b <- d[.(pairs$id2)]
  y <- as.integer(a$entity_id == b$entity_id)

  x_cols <- list()
  for (col in text_cols) {
    cmp <- mv_compare_fields(a[[col]], b[[col]],
                             method    = method,
                             jw_p      = jw_p,
                             fs_breaks = fs_breaks)
    if (method == "fellegi_sunter") {
      x_cols[[paste0(col, "_gamma")]] <- cmp$gamma
    } else {
      if (similarity %in% c("both", "jw")) {
        x_cols[[paste0(col, "_jw")]] <- cmp$jw
      }
      if (similarity %in% c("both", "lv")) {
        x_cols[[paste0(col, "_lv")]] <- cmp$lv
      }
    }
    if (method %in% c("indicator", "full")) {
      x_cols[[paste0(col, "_any_miss")]]  <- as.integer(cmp$any_miss)
      x_cols[[paste0(col, "_both_miss")]] <- as.integer(cmp$both_miss)
    }
  }

  list(X = data.table::as.data.table(x_cols), y = y)
}
