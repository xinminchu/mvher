\name{mv_pair_features}
\alias{mv_pair_features}
\title{Extract pairwise text features with missing-value handling}
\description{
For each candidate pair and each text column, computes Jaro-Winkler and/or
normalised Levenshtein similarity using the chosen missing-value method.
Returns a feature matrix suitable for a binary classifier and a match label
vector.
}
\usage{
mv_pair_features(d, pairs, text_cols,
  method = c("current", "neutral", "zero", "indicator", "full",
             "fellegi_sunter", "complete_cases", "asymmetric"),
  similarity = c("both", "jw", "lv"),
  chunk_size = NULL,
  jw_p = 0.1,
  fs_breaks = c(0.25, 0.60, 0.85))
}
\arguments{
  \item{d}{Record dataset (\code{data.frame}/\code{data.table}) with columns
    \code{id}, \code{entity_id}, and all \code{text_cols}.}
  \item{pairs}{\code{data.frame}/\code{data.table} with columns \code{id1}, \code{id2}.}
  \item{text_cols}{Character vector of column names to use as features.}
  \item{method}{Missing-value handling strategy. See \code{\link{mv_compare_fields}}.}
  \item{similarity}{Which similarities to include: \code{"both"} (default), \code{"jw"}, or \code{"lv"}.}
  \item{chunk_size}{Optional integer. If \code{nrow(pairs) > chunk_size}, computation is chunked.}
  \item{jw_p}{Jaro-Winkler prefix penalty. Default \code{0.1}.}
  \item{fs_breaks}{Fellegi-Sunter gamma-level thresholds. Default \code{c(0.25, 0.60, 0.85)}.}
}
\value{
A list with:
\describe{
  \item{\code{X}}{A \code{data.table} of numeric features (one row per pair).
    Column names follow the pattern \code{<col>_jw}, \code{<col>_lv},
    \code{<col>_gamma}, \code{<col>_any_miss}, \code{<col>_both_miss}.}
  \item{\code{y}}{Integer match label vector (1=same entity, 0=different).}
}
}
\examples{
d <- data.frame(
  id        = 1:4,
  entity_id = c(1L, 1L, 2L, 2L),
  name      = c("Alice", "Alice B.", NA, "Carol")
)
pairs <- data.frame(id1 = c(1, 1, 3), id2 = c(2, 3, 4))
mv_pair_features(d, pairs, text_cols = "name", method = "neutral")
mv_pair_features(d, pairs, text_cols = "name", method = "full")
}
