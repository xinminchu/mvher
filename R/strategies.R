#' Summary table of all missing-value handling strategies
#'
#' Returns a \code{data.frame} describing every strategy supported by
#' \pkg{mvher} along dimensions useful for choosing among them.  Print it
#' to get a quick overview; use \code{\link{mv_recommend}} for a
#' data-driven selection.
#'
#' @return A \code{data.frame} with one row per strategy and columns:
#'   \describe{
#'     \item{\code{strategy}}{Strategy identifier (matches the \code{method} /
#'       \code{strategy} arguments of other functions).}
#'     \item{\code{type}}{One of \code{"field-comparison"},
#'       \code{"imputation"}, or \code{"model-level"}.}
#'     \item{\code{handles_mcar}}{\code{"yes"} / \code{"partial"} / \code{"no"}.}
#'     \item{\code{handles_mar}}{Same coding.}
#'     \item{\code{handles_mnar}}{Same coding.}
#'     \item{\code{adds_features}}{Whether the strategy augments the feature
#'       matrix with extra columns (\code{"yes"} / \code{"no"} /
#'       \code{"gamma"} for Fellegi-Sunter encoding).}
#'     \item{\code{requires_labels}}{Whether entity labels are needed at
#'       fit time.}
#'     \item{\code{computation_cost}}{Relative cost:
#'       \code{"low"} / \code{"medium"} / \code{"high"}.}
#'     \item{\code{native_to_er}}{Whether the strategy originated in the
#'       entity resolution / record linkage literature.}
#'     \item{\code{description}}{Short human-readable description.}
#'   }
#'
#' @export
#' @examples
#' tbl <- mv_strategy_table()
#' print(tbl[tbl$type == "field-comparison", c("strategy","handles_mnar","description")])
mv_strategy_table <- function() {
  data.frame(
    strategy = c(
      # --- field-comparison methods ---
      "current", "neutral", "zero", "asymmetric",
      "indicator", "full", "fellegi_sunter", "complete_cases",
      # --- feature-matrix imputation ---
      "mean", "median", "zero_impute", "half_impute",
      "hot_deck", "knn",
      # --- no-imputation / model-level ---
      "none"
    ),
    type = c(
      rep("field-comparison", 8),
      rep("imputation", 6),
      "model-level"
    ),
    handles_mcar = c(
      "partial","yes","yes","yes","yes","yes","yes","yes",
      "yes","yes","yes","yes","yes","yes","yes"
    ),
    handles_mar = c(
      "no","partial","partial","partial","yes","yes","yes","no",
      "partial","partial","partial","partial","partial","yes","partial"
    ),
    handles_mnar = c(
      "no","no","no","partial","partial","partial","partial","no",
      "no","no","no","no","no","partial","no"
    ),
    adds_features = c(
      "no","no","no","no","yes","yes","gamma","no",
      "no","no","no","no","no","no","no"
    ),
    requires_labels = rep("no", 15),
    computation_cost = c(
      "low","low","low","low","low","low","low","low",
      "low","low","low","low","low","medium","low"
    ),
    native_to_er = c(
      "yes","yes","yes","yes","no","no","yes","yes",
      "no","no","no","no","no","no","yes"
    ),
    description = c(
      "Both-empty \u2192 sim=1 (backward-compat default; semantically wrong for ER)",
      "Set sim=0.5 when either field empty; corresponds to Fellegi-Sunter absent level",
      "Set sim=0.0 when either field empty; conservative penalty for any missingness",
      "0 when both empty, 0.5 when one empty, raw similarity otherwise",
      "Keep raw similarity; add {col}_any_miss and {col}_both_miss binary indicators",
      "Neutral sim=0.5 + indicator columns; best for entity-disjoint splits with correlated missingness",
      "Integer \u03b3-level encoding: 0=missing, 1\u20134=increasing agreement tiers",
      "Set sim=NA for pairs where either field missing; requires downstream complete-case handling",
      "Fill NAs with column mean from training data",
      "Fill NAs with column median; robust to skewed similarity distributions",
      "Fill NAs with 0.0 (corresponds to zero field-comparison method)",
      "Fill NAs with 0.5 (corresponds to neutral field-comparison method)",
      "Fill NAs by random draw from non-missing observed training values",
      "Fill NAs by averaging k nearest complete training rows (Euclidean)",
      "Leave NAs as-is; suitable for tree models or complete-case analysis"
    ),
    stringsAsFactors = FALSE
  )
}


#' Recommend a missing-value handling strategy
#'
#' Uses simple decision rules based on data characteristics to suggest the
#' most appropriate field-comparison method and imputer strategy.  The
#' recommendation errs on the side of caution: when in doubt it favours
#' \code{"full"} (neutral similarity + indicators) because it dominates
#' \code{"neutral"} and \code{"indicator"} separately under entity-disjoint
#' evaluation.
#'
#' @param field_miss_rate Numeric scalar or named vector.  Overall fraction of
#'   records that are missing at least one text field (0–1).  If a vector,
#'   the mean is used.
#' @param entity_corr Numeric scalar (0–1) or \code{NULL}.  Entity-level ICC
#'   from \code{\link{mv_entity_correlation}}.  \code{NULL} or \code{NA} means
#'   unknown.
#' @param has_entity_ids Logical.  Whether entity ground-truth labels are
#'   available at training time.  Default \code{TRUE}.
#' @param split_type Character.  Either \code{"entity"} (entity-disjoint
#'   train/test split) or \code{"record"} (random record split).  Entity-
#'   disjoint splits are more sensitive to entity-correlated missingness.
#'   Default \code{"entity"}.
#'
#' @return A list of class \code{mvher_recommendation} with elements:
#'   \describe{
#'     \item{\code{field_method}}{Recommended field-comparison method
#'       (character string matching \code{\link{mv_pair_features}}'s
#'       \code{method} argument).}
#'     \item{\code{imputer_strategy}}{Recommended imputer strategy
#'       (character string matching \code{\link{mv_fit_imputer}}'s
#'       \code{strategy} argument).}
#'     \item{\code{rationale}}{Human-readable explanation.}
#'   }
#'
#' @seealso \code{\link{mv_strategy_table}}, \code{\link{mv_entity_correlation}},
#'   \code{\link{mv_detect_mechanism}}
#' @export
#' @examples
#' # High entity-correlated missingness under entity-disjoint split
#' mv_recommend(field_miss_rate = 0.25, entity_corr = 0.6, split_type = "entity")
#'
#' # Very low missingness — default is fine
#' mv_recommend(field_miss_rate = 0.01)
#'
#' # Unknown entity correlation, moderate missingness
#' mv_recommend(field_miss_rate = 0.20, entity_corr = NULL)
mv_recommend <- function(field_miss_rate,
                         entity_corr    = NULL,
                         has_entity_ids = TRUE,
                         split_type     = c("entity", "record")) {
  split_type   <- match.arg(split_type)
  overall_miss <- mean(as.numeric(field_miss_rate), na.rm = TRUE)
  corr_val     <- if (is.null(entity_corr) || all(is.na(entity_corr))) 0
                  else mean(as.numeric(entity_corr), na.rm = TRUE)

  # Decision tree
  if (overall_miss < 0.02) {
    field_method     <- "current"
    imputer_strategy <- "mean"
    rationale <- paste0(
      "Very low missingness (<2%): the 'current' default is safe. ",
      "Both-null=1 artefact affects fewer than 0.04% of pairs."
    )

  } else if (split_type == "entity" && corr_val > 0.3) {
    field_method     <- "full"
    imputer_strategy <- "mean"
    rationale <- paste0(
      "Entity-disjoint split with high entity-level ICC (", round(corr_val, 2), "). ",
      "'full' (neutral sim=0.5 + missingness indicators) mitigates covariate ",
      "shift caused by entity-correlated missingness: the model learns a negative ",
      "weight for both_miss instead of receiving a spurious sim=1 signal. ",
      "Mean imputation for any residual NAs in the feature matrix."
    )

  } else if (overall_miss >= 0.15) {
    field_method     <- "neutral"
    imputer_strategy <- "mean"
    rationale <- paste0(
      "Moderate-to-high missingness (", round(100 * overall_miss, 1), "%). ",
      "'neutral' (sim=0.5 on missing pairs) avoids the both-null=1 artefact ",
      "without over-penalising pairs that differ only because one field is absent. ",
      "Consider 'full' if the split is entity-disjoint and ICC > 0.3."
    )

  } else if (corr_val > 0.1 && split_type == "entity") {
    field_method     <- "neutral"
    imputer_strategy <- "mean"
    rationale <- paste0(
      "Low-moderate missingness with some entity-level correlation (ICC=",
      round(corr_val, 2), "). 'neutral' scoring recommended. ",
      "Upgrade to 'full' if evaluation is entity-disjoint and ICC rises above 0.3."
    )

  } else {
    field_method     <- "neutral"
    imputer_strategy <- "mean"
    rationale <- paste0(
      "Low-moderate missingness. 'neutral' scoring (sim=0.5 on missing pairs) ",
      "is a safe, principled default that avoids the both-null=1 artefact."
    )
  }

  rec <- list(
    field_method     = field_method,
    imputer_strategy = imputer_strategy,
    rationale        = rationale
  )
  class(rec) <- "mvher_recommendation"
  rec
}


#' Print an mvher_recommendation object
#'
#' @param x An \code{mvher_recommendation} object from \code{\link{mv_recommend}}.
#' @param ... Unused.
#' @export
print.mvher_recommendation <- function(x, ...) {
  cat("mvher recommendation\n")
  cat("  field_method    :", x$field_method, "\n")
  cat("  imputer_strategy:", x$imputer_strategy, "\n")
  cat("  rationale:\n")
  cat(strwrap(x$rationale, width = 72, prefix = "    "), sep = "\n")
  invisible(x)
}
