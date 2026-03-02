#' mvher: Missing Value Handling for Entity Resolution
#'
#' @description
#' A comprehensive toolkit for handling missing values (NA and empty strings)
#' in entity resolution (ER) and record linkage pipelines.
#'
#' ## Main modules
#'
#' | Module | Functions |
#' |---|---|
#' | **Diagnosis** | [mv_field_summary()], [mv_pair_summary()], [mv_detect_mechanism()], [mv_entity_correlation()] |
#' | **Field comparison** | [mv_compare_fields()], [mv_pair_features()] |
#' | **Imputation** | [mv_fit_imputer()], [mv_apply_imputer()], [mv_impute()] |
#' | **Strategy guidance** | [mv_strategy_table()], [mv_recommend()] |
#' | **Utilities** | [norm_str()] |
#'
#' ## Quick start
#'
#' ```r
#' library(mvher)
#'
#' # Diagnose missingness
#' mv_field_summary(my_records, text_cols = c("name", "address"))
#' mv_detect_mechanism(my_records, text_cols = c("name", "address"))
#'
#' # Get strategy recommendation
#' mv_recommend(field_miss_rate = 0.25, entity_corr = 0.6, split_type = "entity")
#'
#' # Extract pairwise features with 'full' strategy (neutral sim + indicators)
#' feats <- mv_pair_features(records, pairs, text_cols = c("name", "address"),
#'                           method = "full")
#'
#' # Impute feature matrix
#' res <- mv_impute(feats$X, strategy = "mean")
#' ```
#'
#' @references
#' Fellegi, I.P. and Sunter, A.B. (1969). A theory for record linkage.
#' \emph{Journal of the American Statistical Association}, 64(328), 1183--1210.
#'
#' Little, R.J.A. and Rubin, D.B. (2002). \emph{Statistical Analysis with Missing Data},
#' 2nd ed. Wiley.
#'
#' @keywords internal
"_PACKAGE"
