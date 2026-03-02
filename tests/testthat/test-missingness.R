test_that("mv_field_summary returns correct structure", {
  d   <- make_records()
  res <- mv_field_summary(d, c("name", "city"))

  expect_s3_class(res, "data.table")
  expect_equal(nrow(res), 2L)
  expect_true(all(c("column", "n_total", "n_empty", "pct_miss") %in% names(res)))
})

test_that("mv_field_summary counts NA and empty string both as missing", {
  d   <- make_records()
  res <- mv_field_summary(d, "name")
  # name: NA at row 3, "" at row 5  => 2 missing out of 6
  expect_equal(res$n_empty, 2L)
  expect_equal(res$pct_miss, round(100 * 2 / 6, 2))
})

test_that("mv_field_summary errors on missing column", {
  d <- make_records()
  expect_error(mv_field_summary(d, "nonexistent"), "text_cols not found")
})

test_that("mv_pair_summary returns expected structure and totals", {
  d     <- make_records()
  pairs <- make_pairs()
  res   <- mv_pair_summary(d, pairs, c("name", "city"))

  expect_s3_class(res, "data.table")
  expect_true(all(c("column", "type", "n_pairs", "pct_pairs") %in% names(res)))
  # For each column there should be 3 type rows
  expect_equal(nrow(res), 6L)
})

test_that("mv_pair_summary both_missing <= any_missing", {
  d     <- make_records()
  pairs <- make_pairs()
  res   <- mv_pair_summary(d, pairs, "name")
  n_any  <- res[res$type == "any_missing",  "n_pairs"][[1]]
  n_both <- res[res$type == "both_missing", "n_pairs"][[1]]
  n_one  <- res[res$type == "one_missing",  "n_pairs"][[1]]
  expect_true(n_both <= n_any)
  expect_equal(n_one, n_any - n_both)
})

test_that("mv_detect_mechanism returns a data.table with mechanism column", {
  set.seed(42)
  d <- data.frame(
    entity_id = rep(1:5, each = 6),
    name      = ifelse(runif(30) < 0.4, NA_character_, paste0("n", 1:30)),
    stringsAsFactors = FALSE
  )
  res <- mv_detect_mechanism(d, "name")
  expect_s3_class(res, "data.table")
  expect_true("mechanism" %in% names(res))
  expect_true(res$mechanism %in% c("MCAR", "MAR/MNAR", "unknown"))
})

test_that("mv_entity_correlation ICC is in [0, 1]", {
  set.seed(7)
  d <- data.frame(
    entity_id = rep(1:8, each = 5),
    name      = c(rep(NA_character_, 10), paste0("x", 11:40)),
    stringsAsFactors = FALSE
  )
  res <- mv_entity_correlation(d, "name")
  expect_s3_class(res, "data.table")
  expect_true(!is.na(res$icc))
  expect_true(res$icc >= 0 && res$icc <= 1)
})

test_that("norm_str handles NA and trims whitespace", {
  x <- norm_str(c("  Hello ", NA, "FOO  BAR"))
  expect_equal(x, c("hello", "", "foo bar"))
})
