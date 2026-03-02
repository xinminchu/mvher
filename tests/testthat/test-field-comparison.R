test_that("mv_compare_fields current: both-empty gets sim=1", {
  res <- mv_compare_fields("", "", method = "current")
  expect_equal(res$jw, 1)
  expect_equal(res$lv, 1)
})

test_that("mv_compare_fields neutral: any-empty gets sim=0.5", {
  res <- mv_compare_fields(c("Alice", ""), c("Alice", "Bob"), method = "neutral")
  expect_equal(res$jw[2], 0.5)
  expect_equal(res$lv[2], 0.5)
  # non-missing pair is unaffected
  expect_true(res$jw[1] > 0.5)
})

test_that("mv_compare_fields zero: any-empty gets sim=0", {
  res <- mv_compare_fields(c("", "Alice"), c("Bob", "Alice"), method = "zero")
  expect_equal(res$jw[1], 0)
  expect_equal(res$lv[1], 0)
})

test_that("mv_compare_fields indicator: adds no indicator (flags returned separately)", {
  res <- mv_compare_fields(c("Alice", ""), c("Alice", ""), method = "indicator")
  expect_true(any(res$any_miss))
  expect_true(any(res$both_miss))
  # sim is raw, so both-empty → sim=1
  expect_equal(res$jw[2], 1)
})

test_that("mv_compare_fields full: neutral sim + flags", {
  res <- mv_compare_fields(c("Alice", "", "Bob"), c("Alice", "", NA), method = "full")
  # both-empty pair: sim=0.5 (not 1)
  expect_equal(res$jw[2], 0.5)
  expect_true(res$both_miss[2])
  expect_true(res$any_miss[3])  # one is NA
})

test_that("mv_compare_fields fellegi_sunter: returns integer gamma in 0-4", {
  res <- mv_compare_fields(
    c("", "Alice", "Alice", "Alic", "Bob"),
    c("", "Alice", "Alicia", "Xyz",  "Charlie"),
    method = "fellegi_sunter"
  )
  expect_true(is.integer(res$gamma))
  expect_true(all(res$gamma %in% 0:4))
  expect_equal(res$gamma[1], 0L)  # both missing
  expect_equal(res$gamma[2], 4L)  # perfect match
})

test_that("mv_compare_fields complete_cases: missing pairs get NA", {
  res <- mv_compare_fields(c("Alice", ""), c("Alice", "Bob"), method = "complete_cases")
  expect_true(!is.na(res$jw[1]))
  expect_true(is.na(res$jw[2]))
})

test_that("mv_compare_fields asymmetric: both-empty=0, one-empty=0.5", {
  res <- mv_compare_fields(c("", "",     "Alice"),
                           c("", "Bob",  "Bob"),
                           method = "asymmetric")
  expect_equal(res$jw[1], 0)    # both empty → 0
  expect_equal(res$jw[2], 0.5)  # one empty  → 0.5
  expect_true(res$jw[3] >= 0 && res$jw[3] <= 1)  # normal pair
})

test_that("mv_pair_features output has correct dimensions", {
  d     <- make_records()
  pairs <- make_pairs()
  res   <- mv_pair_features(d, pairs, c("name", "city"), method = "neutral")

  expect_type(res$y, "integer")
  expect_equal(length(res$y), nrow(pairs))
  expect_s3_class(res$X, "data.table")
  expect_equal(nrow(res$X), nrow(pairs))
  # both: 2 cols x 2 metrics = 4 feature columns
  expect_equal(ncol(res$X), 4L)
})

test_that("mv_pair_features full method adds indicator columns", {
  d     <- make_records()
  pairs <- make_pairs()
  res   <- mv_pair_features(d, pairs, "name", method = "full")
  expect_true("name_any_miss"  %in% names(res$X))
  expect_true("name_both_miss" %in% names(res$X))
})

test_that("mv_pair_features fellegi_sunter returns gamma columns", {
  d     <- make_records()
  pairs <- make_pairs()
  res   <- mv_pair_features(d, pairs, "name", method = "fellegi_sunter")
  expect_true("name_gamma" %in% names(res$X))
  expect_true(is.integer(res$X$name_gamma))
})

test_that("mv_pair_features jw-only: half the feature columns", {
  d     <- make_records()
  pairs <- make_pairs()
  res_b <- mv_pair_features(d, pairs, "name", method = "neutral", similarity = "both")
  res_j <- mv_pair_features(d, pairs, "name", method = "neutral", similarity = "jw")
  expect_equal(ncol(res_j$X), ncol(res_b$X) / 2)
})

test_that("mv_pair_features empty pairs returns empty output", {
  d     <- make_records()
  empty <- data.frame(id1 = integer(), id2 = integer())
  res   <- mv_pair_features(d, empty, "name", method = "neutral")
  expect_equal(nrow(res$X), 0L)
  expect_equal(length(res$y), 0L)
})

test_that("mv_pair_features chunked output equals non-chunked", {
  d     <- make_records()
  pairs <- make_pairs()
  r1 <- mv_pair_features(d, pairs, "name", method = "neutral", chunk_size = NULL)
  r2 <- mv_pair_features(d, pairs, "name", method = "neutral", chunk_size = 2L)
  expect_equal(as.data.frame(r1$X), as.data.frame(r2$X))
  expect_equal(r1$y, r2$y)
})
