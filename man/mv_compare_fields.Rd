\name{mv_compare_fields}
\alias{mv_compare_fields}
\title{Pairwise field comparison with missing-value handling}
\description{
Computes Jaro-Winkler (JW) and normalised Levenshtein (LV) string
similarity between two character vectors, with configurable treatment of
empty/NA fields via eight distinct strategies.
}
\usage{
mv_compare_fields(xa, xb,
  method = c("current", "neutral", "zero", "indicator", "full",
             "fellegi_sunter", "complete_cases", "asymmetric"),
  jw_p = 0.1,
  fs_breaks = c(0.25, 0.60, 0.85))
}
\arguments{
  \item{xa, xb}{Character vectors of equal length (one entry per candidate pair).}
  \item{method}{Missing-value handling strategy (see Details).}
  \item{jw_p}{Jaro-Winkler prefix penalty. Default \code{0.1}.}
  \item{fs_breaks}{Length-3 numeric vector of JW thresholds for Fellegi-Sunter
    gamma-level tier boundaries. Default \code{c(0.25, 0.60, 0.85)}.}
}
\details{
\describe{
  \item{\code{"current"}}{Compute similarity as-is.  Both-empty pairs receive
    sim=1 (semantically wrong but backward-compatible).}
  \item{\code{"neutral"}}{Set sim=0.5 when either field is empty (Fellegi-Sunter
    absent level — neither supports nor contradicts a match).}
  \item{\code{"zero"}}{Set sim=0.0 when either field is empty.}
  \item{\code{"indicator"}}{Keep raw similarity; set \code{any_miss} and
    \code{both_miss} flags for downstream indicator column creation.}
  \item{\code{"full"}}{Neutral sim=0.5 + indicator flags.}
  \item{\code{"fellegi_sunter"}}{Integer gamma-level: 0=missing, 1-4=agreement tiers.}
  \item{\code{"complete_cases"}}{Set sim=NA when either field is empty.}
  \item{\code{"asymmetric"}}{0 when both empty; 0.5 when one empty; raw otherwise.}
}
}
\value{
A named list with elements \code{jw}, \code{lv}, \code{any_miss},
\code{both_miss}, and (for \code{"fellegi_sunter"} only) \code{gamma}.
}
\examples{
xa <- c("Alice",  "Bob",  "",  "Carol")
xb <- c("Alice B","Bobby","",  NA)
mv_compare_fields(xa, xb, method = "full")
mv_compare_fields(xa, xb, method = "fellegi_sunter")
}
