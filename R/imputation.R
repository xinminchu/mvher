#' Fit a feature-matrix imputer
#'
#' Learns imputation fill values from a training feature matrix.  The fitted
#' object can then be applied to both training and test matrices via
#' \code{\link{mv_apply_imputer}}, ensuring that imputation parameters are
#' derived from training data only (no data leakage).
#'
#' @param X A \code{data.frame} or \code{data.table} of numeric features
#'   (one column per feature, one row per candidate pair).
#' @param strategy Imputation strategy.  One of:
#'   \describe{
#'     \item{\code{"mean"}}{Fill NAs with the column mean.  Default.}
#'     \item{\code{"median"}}{Fill NAs with the column median.  More robust
#'       to skewed similarity distributions.}
#'     \item{\code{"zero"}}{Fill NAs with 0.0.  Treats absent comparisons as
#'       non-matching.}
#'     \item{\code{"half"}}{Fill NAs with 0.5.  Treats absent comparisons as
#'       uninformative (neutral probability).}
#'     \item{\code{"hot_deck"}}{For each missing value, draw a replacement at
#'       random from the non-missing observed values for that column.  Preserves
#'       the marginal distribution.}
#'     \item{\code{"knn"}}{k-nearest-neighbour imputation.  The k nearest
#'       complete rows from the training set (Euclidean distance on available
#'       columns) are averaged to fill each missing value.}
#'     \item{\code{"none"}}{No imputation.  NAs are left as-is.  Useful when
#'       the downstream model handles missing values natively (e.g., trees).}
#'   }
#' @param k Number of neighbours for \code{"knn"} imputation.  Default \code{5L}.
#' @param seed Integer random seed for \code{"hot_deck"} and \code{"knn"}.
#'   Default \code{1L}.
#'
#' @return An object of class \code{mvher_imputer} (a named list) with elements:
#'   \describe{
#'     \item{\code{strategy}}{The chosen strategy string.}
#'     \item{\code{fills}}{Learned fill parameters (strategy-dependent).}
#'     \item{\code{col_names}}{Column names of the training matrix.}
#'     \item{\code{k}}{The k parameter (for \code{"knn"}).}
#'     \item{\code{seed}}{The seed used.}
#'   }
#'
#' @seealso \code{\link{mv_apply_imputer}}, \code{\link{mv_impute}}
#' @export
#' @examples
#' X <- data.frame(a = c(0.8, NA, 0.3, 0.9),
#'                 b = c(NA, 0.6, 0.7, 0.5))
#' imp_mean <- mv_fit_imputer(X, strategy = "mean")
#' imp_knn  <- mv_fit_imputer(X, strategy = "knn", k = 2L)
#' mv_apply_imputer(X, imp_mean)
mv_fit_imputer <- function(X,
                           strategy = c("mean", "median", "zero", "half",
                                        "hot_deck", "knn", "none"),
                           k    = 5L,
                           seed = 1L) {
  if (identical(strategy, "zero_impute")) {
    warning("strategy=\"zero_impute\" is deprecated; use \"zero\"",
            call. = FALSE)
    strategy <- "zero"
  } else if (identical(strategy, "half_impute")) {
    warning("strategy=\"half_impute\" is deprecated; use \"half\"",
            call. = FALSE)
    strategy <- "half"
  }
  strategy  <- match.arg(strategy)
  X         <- as.data.frame(X)
  col_names <- names(X)

  fills <- switch(strategy,

    mean = {
      mu <- vapply(X, function(col) {
        v <- mean(col, na.rm = TRUE); if (is.nan(v) || is.na(v)) 0 else v
      }, numeric(1))
      as.list(mu)
    },

    median = {
      mu <- vapply(X, function(col) {
        v <- median(col, na.rm = TRUE); if (is.na(v)) 0 else v
      }, numeric(1))
      as.list(mu)
    },

    zero = setNames(as.list(rep(0.0, length(col_names))), col_names),

    half = setNames(as.list(rep(0.5, length(col_names))), col_names),

    hot_deck = {
      lapply(X, function(col) {
        obs <- col[!is.na(col)]
        if (length(obs) == 0) numeric(0) else obs
      })
    },

    knn = {
      complete  <- X[complete.cases(X), , drop = FALSE]
      col_means <- vapply(X, function(col) {
        v <- mean(col, na.rm = TRUE)
        if (is.nan(v) || is.na(v)) 0 else v
      }, numeric(1))
      list(complete_rows = complete,
           col_means     = col_means,
           k             = as.integer(k),
           seed          = as.integer(seed))
    },

    none = setNames(as.list(rep(NA_real_, length(col_names))), col_names)
  )

  structure(
    list(strategy  = strategy,
         fills     = fills,
         col_names = col_names,
         k         = as.integer(k),
         seed      = as.integer(seed)),
    class = "mvher_imputer"
  )
}


#' Apply a fitted imputer to a feature matrix
#'
#' Fills \code{NA} values in \code{X} using parameters learned by
#' \code{\link{mv_fit_imputer}}.  Apply the *same* fitted object to both the
#' training matrix and any test matrix to avoid data leakage.
#'
#' @param X A \code{data.frame} or \code{data.table} of numeric features.
#' @param imputer An \code{mvher_imputer} object from
#'   \code{\link{mv_fit_imputer}}.
#'
#' @return A \code{data.table} with the same dimensions as \code{X} but with
#'   \code{NA}s replaced according to the strategy stored in \code{imputer}.
#'
#' @importFrom data.table copy as.data.table set
#' @seealso \code{\link{mv_fit_imputer}}, \code{\link{mv_impute}}
#' @export
#' @examples
#' X  <- data.frame(a = c(0.8, NA, 0.3), b = c(NA, 0.6, 0.7))
#' Xt <- data.frame(a = c(NA, 0.5),      b = c(0.4, NA))
#' imp <- mv_fit_imputer(X, strategy = "mean")
#' mv_apply_imputer(X,  imp)  # training matrix
#' mv_apply_imputer(Xt, imp)  # test matrix (uses training means)
mv_apply_imputer <- function(X, imputer) {
  if (!inherits(imputer, "mvher_imputer")) {
    stop("imputer must be an mvher_imputer object from mv_fit_imputer()", call. = FALSE)
  }
  X2       <- data.table::copy(data.table::as.data.table(X))
  strategy <- imputer$strategy
  fills    <- imputer$fills

  if (strategy == "none") return(X2)

  if (strategy %in% c("mean", "median", "zero", "half")) {
    for (nm in names(fills)) {
      if (!nm %in% names(X2)) next
      nas <- which(is.na(X2[[nm]]))
      if (length(nas) > 0) data.table::set(X2, nas, nm, fills[[nm]])
    }
    return(X2)
  }

  if (strategy == "hot_deck") {
    set.seed(imputer$seed)
    for (nm in names(fills)) {
      if (!nm %in% names(X2)) next
      nas <- which(is.na(X2[[nm]]))
      if (length(nas) == 0) next
      obs <- fills[[nm]]
      fill_val <- if (length(obs) == 0) 0 else
                  sample(obs, length(nas), replace = TRUE)
      data.table::set(X2, nas, nm, fill_val)
    }
    return(X2)
  }

  if (strategy == "knn") {
    complete_rows <- fills$complete_rows
    k_use         <- fills$k %||% imputer$k
    col_means     <- fills$col_means %||%
                     setNames(rep(0, ncol(X2)), names(X2))

    if (is.null(complete_rows) || nrow(complete_rows) == 0) {
      # Fallback: column-mean fill when no complete training rows exist
      for (nm in names(X2)) {
        nas <- which(is.na(X2[[nm]]))
        fv  <- if (nm %in% names(col_means)) col_means[[nm]] else 0
        if (length(nas) > 0) data.table::set(X2, nas, nm, fv)
      }
      return(X2)
    }

    # Align training matrix columns to those present in X2
    train_cols  <- colnames(complete_rows)
    shared_cols <- intersect(train_cols, names(X2))
    if (length(shared_cols) == 0L) {
      warning("knn: no shared columns between training and imputation data; ",
              "falling back to column means.", call. = FALSE)
      for (nm in names(X2)) {
        nas <- which(is.na(X2[[nm]]))
        fv  <- if (nm %in% names(col_means)) col_means[[nm]] else 0
        if (length(nas) > 0) data.table::set(X2, nas, nm, fv)
      }
      return(X2)
    }
    if (!identical(shared_cols, train_cols)) {
      warning("knn: column mismatch between training and imputation data; ",
              "distance computed on shared columns only.", call. = FALSE)
    }
    complete_rows_shared <- complete_rows[, shared_cols, drop = FALSE]

    fill_counts        <- integer(ncol(X2))
    names(fill_counts) <- names(X2)
    fallback_count     <- 0L

    incomplete_rows <- which(!complete.cases(as.data.frame(X2)))
    X2_df   <- as.data.frame(X2)
    cmp_mat <- as.matrix(complete_rows_shared)

    for (ri in incomplete_rows) {
      # Use full X2 row to find which cols are NA; subset to shared for distance
      row_full  <- as.numeric(X2_df[ri, , drop = TRUE])
      names(row_full) <- names(X2)
      na_nms    <- names(X2)[is.na(row_full)]   # names of NA cols in X2
      row_shared <- row_full[shared_cols]
      avail_shared <- shared_cols[!is.na(row_shared)]

      if (length(na_nms) == 0) next

      if (length(avail_shared) == 0) {
        # All shared cols NA: fall back to column means
        for (nm in na_nms) {
          fv <- if (nm %in% names(col_means)) col_means[[nm]] else 0
          data.table::set(X2, ri, nm, fv)
          fill_counts[[nm]] <- fill_counts[[nm]] + 1L
        }
        fallback_count <- fallback_count + 1L
        next
      }

      cmp_sub <- cmp_mat[, avail_shared, drop = FALSE]
      row_sub <- row_shared[avail_shared]
      dists   <- rowSums(sweep(cmp_sub, 2, row_sub, "-")^2)
      nn_idx  <- order(dists)[seq_len(min(k_use, length(dists)))]
      # imp_row is named by shared_cols; use name-based lookup below
      imp_row <- colMeans(cmp_mat[nn_idx, , drop = FALSE], na.rm = TRUE)

      for (nm in na_nms) {
        fv <- if (nm %in% names(imp_row) &&
                  !is.nan(imp_row[[nm]]) && !is.na(imp_row[[nm]]))
                imp_row[[nm]]
              else if (nm %in% names(col_means))
                col_means[[nm]]
              else 0
        data.table::set(X2, ri, nm, fv)
        fill_counts[[nm]] <- fill_counts[[nm]] + 1L
      }
    }

    # Final backstop: fill any residual NAs with column means
    for (nm in names(X2)) {
      nas <- which(is.na(X2[[nm]]))
      if (length(nas) > 0) {
        fv <- if (nm %in% names(col_means)) col_means[[nm]] else 0
        data.table::set(X2, nas, nm, fv)
        fill_counts[[nm]] <- fill_counts[[nm]] + length(nas)
      }
    }

    attr(X2, "imputation_report") <- list(
      fill_counts    = fill_counts,
      fallback_count = fallback_count
    )
    return(X2)
  }

  X2   # should not reach here
}


#' Fit and apply imputation in one step
#'
#' Convenience wrapper: fits an imputer on \code{X_train} and applies it to
#' both \code{X_train} and (optionally) \code{X_test} in one call.
#'
#' @param X_train Training feature matrix.
#' @param X_test Optional test feature matrix.  If supplied, it is imputed
#'   using parameters learned from \code{X_train} (no leakage).
#' @param strategy Imputation strategy.  See \code{\link{mv_fit_imputer}}.
#' @param k Number of neighbours for \code{"knn"}.  Default \code{5L}.
#' @param seed Random seed.  Default \code{1L}.
#'
#' @return A list with:
#'   \describe{
#'     \item{\code{X_train}}{Imputed training matrix (\code{data.table}).}
#'     \item{\code{X_test}}{Imputed test matrix (\code{data.table}), or
#'       \code{NULL} if \code{X_test} was not supplied.}
#'     \item{\code{imputer}}{The fitted \code{mvher_imputer} object, so the
#'       caller can apply it to additional data later.}
#'   }
#'
#' @seealso \code{\link{mv_fit_imputer}}, \code{\link{mv_apply_imputer}}
#' @export
#' @examples
#' Xtr <- data.frame(a = c(0.9, NA, 0.2), b = c(0.7, 0.8, NA))
#' Xte <- data.frame(a = c(NA, 0.5),      b = c(0.6, NA))
#' res <- mv_impute(Xtr, Xte, strategy = "median")
#' res$X_train
#' res$X_test
mv_impute <- function(X_train, X_test = NULL,
                      strategy = c("mean", "median", "zero", "half",
                                   "hot_deck", "knn", "none"),
                      k    = 5L,
                      seed = 1L) {
  if (identical(strategy, "zero_impute")) {
    warning("strategy=\"zero_impute\" is deprecated; use \"zero\"",
            call. = FALSE)
    strategy <- "zero"
  } else if (identical(strategy, "half_impute")) {
    warning("strategy=\"half_impute\" is deprecated; use \"half\"",
            call. = FALSE)
    strategy <- "half"
  }
  strategy <- match.arg(strategy)
  imp      <- mv_fit_imputer(X_train, strategy = strategy, k = k, seed = seed)
  Xtr_imp  <- mv_apply_imputer(X_train, imp)
  Xte_imp  <- if (!is.null(X_test)) mv_apply_imputer(X_test, imp) else NULL
  list(X_train = Xtr_imp, X_test = Xte_imp, imputer = imp)
}


#' Print an mvher_imputer object
#'
#' @param x An \code{mvher_imputer} object from \code{\link{mv_fit_imputer}}.
#' @param ... Unused.
#' @export
print.mvher_imputer <- function(x, ...) {
  cat("mvher_imputer\n")
  cat("  strategy:", x$strategy, "\n")
  cat("  columns :", length(x$col_names), "\n")
  if (x$strategy == "knn")
    cat("  k       :", x$k, "\n")
  invisible(x)
}
