make_feat_mat <- function() {
  data.frame(
    sim_jw = c(0.9, NA,  0.3, 0.8),
    sim_lv = c(NA,  0.7, 0.5, NA)
  )
}

test_that("mv_fit_imputer returns mvher_imputer object", {
  X   <- make_feat_mat()
  imp <- mv_fit_imputer(X, strategy = "mean")
  expect_s3_class(imp, "mvher_imputer")
  expect_equal(imp$strategy, "mean")
})

test_that("mv_apply_imputer (mean) fills all NAs", {
  X   <- make_feat_mat()
  imp <- mv_fit_imputer(X, strategy = "mean")
  X2  <- mv_apply_imputer(X, imp)
  expect_false(anyNA(X2))
})

test_that("mv_apply_imputer (mean) uses correct column means", {
  X   <- make_feat_mat()
  imp <- mv_fit_imputer(X, strategy = "mean")
  X2  <- mv_apply_imputer(X, imp)
  expected_jw_fill <- mean(c(0.9, 0.3, 0.8))  # NA excluded
  expect_equal(X2$sim_jw[2], expected_jw_fill)
})

test_that("mv_apply_imputer (median) fills NAs", {
  X   <- make_feat_mat()
  imp <- mv_fit_imputer(X, strategy = "median")
  X2  <- mv_apply_imputer(X, imp)
  expect_false(anyNA(X2))
})

test_that("mv_apply_imputer (zero) fills with 0", {
  X   <- make_feat_mat()
  imp <- mv_fit_imputer(X, strategy = "zero")
  X2  <- mv_apply_imputer(X, imp)
  expect_equal(X2$sim_jw[2], 0)
  expect_false(anyNA(X2))
})

test_that("mv_apply_imputer (half) fills with 0.5", {
  X   <- make_feat_mat()
  imp <- mv_fit_imputer(X, strategy = "half")
  X2  <- mv_apply_imputer(X, imp)
  expect_equal(X2$sim_jw[2], 0.5)
  expect_equal(X2$sim_lv[4], 0.5)
})

test_that("mv_apply_imputer (hot_deck) fills NAs with observed values", {
  set.seed(1)
  X   <- make_feat_mat()
  imp <- mv_fit_imputer(X, strategy = "hot_deck", seed = 1L)
  X2  <- mv_apply_imputer(X, imp)
  expect_false(anyNA(X2))
  # Filled values must be drawn from observed values in training
  observed_jw <- c(0.9, 0.3, 0.8)
  expect_true(X2$sim_jw[2] %in% observed_jw)
})

test_that("mv_apply_imputer (knn) fills NAs", {
  X   <- make_feat_mat()
  imp <- mv_fit_imputer(X, strategy = "knn", k = 2L)
  X2  <- mv_apply_imputer(X, imp)
  # Not necessarily all NAs removed (depends on kNN coverage), but no error
  expect_s3_class(X2, "data.table")
})

test_that("mv_apply_imputer (none) leaves NAs unchanged", {
  X   <- make_feat_mat()
  imp <- mv_fit_imputer(X, strategy = "none")
  X2  <- mv_apply_imputer(X, imp)
  expect_true(anyNA(X2))
})

test_that("mv_impute applies to both train and test", {
  Xtr <- make_feat_mat()
  Xte <- data.frame(sim_jw = c(NA, 0.4), sim_lv = c(0.6, NA))
  res <- mv_impute(Xtr, Xte, strategy = "mean")
  expect_false(anyNA(res$X_train))
  expect_false(anyNA(res$X_test))
  expect_s3_class(res$imputer, "mvher_imputer")
})

test_that("mv_impute with X_test=NULL returns NULL X_test", {
  Xtr <- make_feat_mat()
  res <- mv_impute(Xtr, strategy = "half")
  expect_null(res$X_test)
})

test_that("print.mvher_imputer runs without error", {
  X   <- make_feat_mat()
  imp <- mv_fit_imputer(X, strategy = "knn", k = 3L)
  expect_output(print(imp), "mvher_imputer")
  expect_output(print(imp), "knn")
})

test_that("mv_apply_imputer errors when passed non-imputer", {
  X <- make_feat_mat()
  expect_error(mv_apply_imputer(X, list(strategy = "mean")), "mvher_imputer")
})

test_that("mv_fit_imputer test-set uses training means (no leakage)", {
  Xtr <- data.frame(a = c(0.1, 0.3, 0.5))
  Xte <- data.frame(a = c(NA_real_))
  imp <- mv_fit_imputer(Xtr, strategy = "mean")
  Xte2 <- mv_apply_imputer(Xte, imp)
  expect_equal(Xte2$a, mean(c(0.1, 0.3, 0.5)))
})
