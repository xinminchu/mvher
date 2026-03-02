\name{mv_detect_mechanism}
\alias{mv_detect_mechanism}
\title{Estimate the missingness mechanism (MCAR / MAR / MNAR heuristic)}
\description{
Tests for association between missingness and entity membership using a
chi-squared test of independence.  High association suggests MAR or MNAR;
low association is consistent with MCAR.
}
\usage{
mv_detect_mechanism(d, text_cols, entity_col = "entity_id", alpha = 0.05)
}
\arguments{
  \item{d}{Record dataset containing \code{entity_col} and all \code{text_cols}.}
  \item{text_cols}{Character vector of column names to test.}
  \item{entity_col}{Name of the entity column. Default \code{"entity_id"}.}
  \item{alpha}{Significance level for the chi-squared test. Default \code{0.05}.}
}
\value{
A \code{data.table} with columns \code{column}, \code{overall_miss_rate},
\code{entity_miss_variance}, \code{chi_sq_p}, \code{mechanism}.
}
\details{
A significant chi-squared p-value indicates that missingness is associated
with entity identity (MAR or MNAR).  It cannot distinguish MAR from MNAR
without external covariates.
}
\examples{
set.seed(1)
d <- data.frame(
  entity_id = rep(1:5, each = 4),
  name = ifelse(runif(20) < 0.3, NA_character_, paste0("name", 1:20))
)
mv_detect_mechanism(d, "name")
}
