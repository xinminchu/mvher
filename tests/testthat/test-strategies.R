# ---- mv_strategy_table() -----------------------------------------------

test_that("mv_strategy_table returns a data.frame with expected columns", {
  tbl <- mv_strategy_table()
  expect_s3_class(tbl, "data.frame")
  expect_true(all(c("strategy", "type", "handles_mcar",
                    "handles_mar", "handles_mnar",
                    "adds_features", "requires_labels",
                    "computation_cost", "native_to_er",
                    "description") %in% names(tbl)))
})

test_that("mv_strategy_table imputation strategies match mv_fit_imputer names", {
  tbl     <- mv_strategy_table()
  imp_tbl <- tbl[tbl$type == "imputation", "strategy"]
  valid   <- c("mean", "median", "zero", "half", "hot_deck", "knn")
  # All imputation strategies must be accepted by mv_fit_imputer
  for (s in setdiff(imp_tbl, "none")) {
    expect_true(s %in% valid,
                info = paste0("'", s, "' not a valid mv_fit_imputer strategy"))
  }
})

test_that("mv_strategy_table uses 'zero'/'half', not old 'zero_impute'/'half_impute'", {
  tbl <- mv_strategy_table()
  expect_false("zero_impute" %in% tbl$strategy)
  expect_false("half_impute" %in% tbl$strategy)
  expect_true("zero" %in% tbl$strategy)
  expect_true("half" %in% tbl$strategy)
})

test_that("mv_strategy_table field-comparison strategies match mv_compare_fields", {
  tbl    <- mv_strategy_table()
  fc_tbl <- tbl[tbl$type == "field-comparison", "strategy"]
  valid  <- c("current", "neutral", "zero", "asymmetric",
              "indicator", "full", "fellegi_sunter", "complete_cases")
  for (s in fc_tbl) {
    expect_true(s %in% valid,
                info = paste0("'", s, "' not a valid mv_compare_fields method"))
  }
})


# ---- mv_recommend() -------------------------------------------------------

test_that("mv_recommend returns mvher_recommendation with required elements", {
  rec <- mv_recommend(field_miss_rate = 0.1)
  expect_s3_class(rec, "mvher_recommendation")
  expect_true(all(c("field_method", "imputer_strategy", "rationale") %in%
                    names(rec)))
})

test_that("mv_recommend: very low missingness -> 'current'", {
  rec <- mv_recommend(field_miss_rate = 0.01)
  expect_equal(rec$field_method, "current")
  expect_equal(rec$imputer_strategy, "mean")
})

test_that("mv_recommend: high ICC + entity split -> 'full'", {
  rec <- mv_recommend(field_miss_rate = 0.20, entity_corr = 0.7,
                      has_entity_ids = TRUE, split_type = "entity")
  expect_equal(rec$field_method, "full")
})

test_that("mv_recommend: high ICC but no entity IDs -> not 'full'", {
  # Without entity labels we cannot verify ICC; recommender should not pick 'full'
  rec <- mv_recommend(field_miss_rate = 0.20, entity_corr = 0.7,
                      has_entity_ids = FALSE, split_type = "entity")
  expect_false(rec$field_method == "full")
})

test_that("mv_recommend: moderate missingness -> 'neutral'", {
  rec <- mv_recommend(field_miss_rate = 0.20)
  expect_equal(rec$field_method, "neutral")
})

test_that("mv_recommend: entity_corr=NULL is handled without error", {
  expect_no_error(mv_recommend(field_miss_rate = 0.20, entity_corr = NULL))
})

test_that("mv_recommend: entity_corr=NA is handled without error", {
  expect_no_error(mv_recommend(field_miss_rate = 0.20, entity_corr = NA_real_))
})

test_that("mv_recommend: high missingness (>=30%) -> knn imputer", {
  rec <- mv_recommend(field_miss_rate = 0.35)
  expect_equal(rec$imputer_strategy, "knn")
})

test_that("mv_recommend: low missingness -> mean imputer", {
  rec <- mv_recommend(field_miss_rate = 0.10)
  expect_equal(rec$imputer_strategy, "mean")
})

test_that("mv_recommend: record split does not trigger 'full'", {
  rec <- mv_recommend(field_miss_rate = 0.25, entity_corr = 0.8,
                      split_type = "record")
  expect_false(rec$field_method == "full")
})

test_that("print.mvher_recommendation runs without error", {
  rec <- mv_recommend(field_miss_rate = 0.2, entity_corr = 0.5)
  expect_output(print(rec), "field_method")
  expect_output(print(rec), "imputer_strategy")
})


# ---- Input validation across functions ------------------------------------

test_that("mv_pair_features errors on duplicate IDs in d", {
  d_dup <- data.frame(
    id        = c(1L, 1L, 2L),
    entity_id = c(1L, 1L, 2L),
    name      = c("Alice", "Alice", "Bob"),
    stringsAsFactors = FALSE
  )
  p <- data.frame(id1 = 1L, id2 = 2L)
  expect_error(mv_pair_features(d_dup, p, "name"), "duplicate")
})

test_that("mv_pair_features errors when pairs reference unknown IDs", {
  d <- make_records()
  p <- data.frame(id1 = c(1L, 99L), id2 = c(2L, 3L))
  expect_error(mv_pair_features(d, p, "name"), "not found")
})

test_that("mv_compare_fields errors on invalid fs_breaks (wrong length)", {
  expect_error(
    mv_compare_fields("a", "b", method = "fellegi_sunter",
                      fs_breaks = c(0.3, 0.7)),
    "length 3"
  )
})

test_that("mv_compare_fields errors on non-increasing fs_breaks", {
  expect_error(
    mv_compare_fields("a", "b", method = "fellegi_sunter",
                      fs_breaks = c(0.7, 0.3, 0.9)),
    "strictly increasing"
  )
})

test_that("mv_compare_fields errors when fs_breaks outside (0,1)", {
  expect_error(
    mv_compare_fields("a", "b", method = "fellegi_sunter",
                      fs_breaks = c(0, 0.5, 0.9)),
    "open interval"
  )
})

test_that("mv_fit_imputer deprecated 'zero_impute' warns and works", {
  X <- data.frame(a = c(0.5, NA))
  expect_warning(
    imp <- mv_fit_imputer(X, strategy = "zero_impute"),
    "deprecated"
  )
  expect_equal(imp$strategy, "zero")
})

test_that("mv_fit_imputer deprecated 'half_impute' warns and works", {
  X <- data.frame(a = c(0.5, NA))
  expect_warning(
    imp <- mv_fit_imputer(X, strategy = "half_impute"),
    "deprecated"
  )
  expect_equal(imp$strategy, "half")
})

test_that("mv_apply_imputer knn: all-NA row is filled (no residual NAs)", {
  X <- data.frame(
    a = c(0.8,  0.2, NA),
    b = c(0.6,  0.4, NA)
  )
  imp <- mv_fit_imputer(X, strategy = "knn", k = 2L)
  X2  <- mv_apply_imputer(X, imp)
  expect_false(anyNA(X2))
})

test_that("mv_apply_imputer knn: imputation_report attribute is present", {
  X   <- data.frame(a = c(0.9, NA, 0.1), b = c(NA, 0.5, 0.3))
  imp <- mv_fit_imputer(X, strategy = "knn", k = 2L)
  X2  <- mv_apply_imputer(X, imp)
  rpt <- attr(X2, "imputation_report")
  expect_type(rpt, "list")
  expect_true("fill_counts"    %in% names(rpt))
  expect_true("fallback_count" %in% names(rpt))
})
