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
      "mean", "median", "zero", "half",
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
#' @param classifier Character.  Either \code{"linear"} (default, covers
#'   logistic regression, SVMs, neural networks---any model that requires a
#'   complete numeric feature matrix) or \code{"tree_native"} (tree-based
#'   models such as \pkg{xgboost}, \pkg{lightgbm}, and \pkg{ranger} that
#'   handle \code{NA}s natively via learned split directions or surrogate
#'   splits).  When \code{"tree_native"}, the recommended
#'   \code{imputer_strategy} is always \code{"none"}: residual \code{NA}s in
#'   the feature matrix are left in place for the classifier to handle.
#'   The field-comparison method is still chosen to avoid the both-null
#'   artifact.
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
#'
#' # Native-MV tree classifier (e.g. xgboost) -- no imputation needed
#' mv_recommend(field_miss_rate = 0.25, entity_corr = 0.6,
#'              split_type = "entity", classifier = "tree_native")
mv_recommend <- function(field_miss_rate,
                         entity_corr    = NULL,
                         has_entity_ids = TRUE,
                         split_type     = c("entity", "record"),
                         classifier     = c("linear", "tree_native")) {
  split_type          <- match.arg(split_type)
  classifier          <- match.arg(classifier)
  overall_miss        <- mean(as.numeric(field_miss_rate), na.rm = TRUE)
  entity_corr_unknown <- is.null(entity_corr) || all(is.na(entity_corr))
  corr_val            <- if (entity_corr_unknown) 0
                         else mean(as.numeric(entity_corr), na.rm = TRUE)

  # Imputer: knn for high missingness (better MAR handling), mean otherwise
  base_imputer <- if (overall_miss >= 0.30) "knn" else "mean"

  # Decision tree
  if (overall_miss < 0.02) {
    field_method     <- "current"
    imputer_strategy <- "mean"
    rationale <- paste0(
      "Very low missingness (<2%): the 'current' default is safe. ",
      "Assuming independent missingness, fewer than ~0.04% of pairs ",
      "are affected by the both-null=1 artifact."
    )

  } else if (split_type == "entity" && has_entity_ids &&
             !entity_corr_unknown && corr_val > 0.3) {
    field_method     <- "full"
    imputer_strategy <- base_imputer
    rationale <- paste0(
      "Entity-disjoint split with high entity-level ICC (",
      round(corr_val, 2), "). ",
      "'full' (neutral sim=0.5 + missingness indicators) mitigates covariate ",
      "shift caused by entity-correlated missingness: the model learns a ",
      "negative weight for both_miss instead of receiving a spurious sim=1 ",
      "signal. ",
      if (base_imputer == "knn")
        "KNN imputation recommended for high overall missingness (>=30%)."
      else
        "Mean imputation for any residual NAs in the feature matrix."
    )

  } else if (overall_miss >= 0.15) {
    field_method     <- "neutral"
    imputer_strategy <- base_imputer
    rationale <- paste0(
      "Moderate-to-high missingness (", round(100 * overall_miss, 1), "%). ",
      "'neutral' (sim=0.5 on missing pairs) avoids the both-null=1 artifact ",
      "without over-penalising pairs that differ only because one field is ",
      "absent. ",
      if (!has_entity_ids)
        "Entity labels absent; ICC cannot be estimated. "
      else if (entity_corr_unknown)
        "ICC unknown; "
      else
        paste0("ICC=", round(corr_val, 2), "; "),
      "Consider 'full' if the split is entity-disjoint and ICC > 0.3.",
      if (base_imputer == "knn")
        " KNN imputation recommended for high missingness (>=30%)."
    )

  } else if (!entity_corr_unknown && corr_val > 0.1 &&
             split_type == "entity") {
    field_method     <- "neutral"
    imputer_strategy <- base_imputer
    rationale <- paste0(
      "Low-moderate missingness with some entity-level correlation (ICC=",
      round(corr_val, 2), "). 'neutral' scoring recommended. ",
      "Upgrade to 'full' if evaluation is entity-disjoint and ICC rises ",
      "above 0.3."
    )

  } else {
    field_method     <- "neutral"
    imputer_strategy <- base_imputer
    rationale <- paste0(
      "Low-moderate missingness. 'neutral' scoring (sim=0.5 on missing ",
      "pairs) is a safe, principled default that avoids the both-null=1 ",
      "artifact.",
      if (entity_corr_unknown && has_entity_ids)
        " ICC unknown; run mv_entity_correlation() to refine this recommendation."
      else if (!has_entity_ids)
        " Entity labels absent; ICC-based refinement not possible."
      else
        ""
    )
  }

  # Native-MV classifiers handle NAs themselves; skip imputation entirely
  if (classifier == "tree_native") {
    imputer_strategy <- "none"
    rationale <- paste0(
      "Native-MV classifier requested (e.g. xgboost, lightgbm, ranger): ",
      "imputation skipped (strategy = 'none'). ",
      "Residual NAs in the feature matrix are left for the classifier's ",
      "native missing-value mechanism. ",
      "Field-comparison method '", field_method, "' still applied to ",
      "avoid the both-null=1 artifact at the similarity-computation stage."
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
