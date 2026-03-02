\name{mv_entity_correlation}
\alias{mv_entity_correlation}
\title{Measure entity-correlated missingness via one-way ICC}
\description{
Computes the intra-class correlation (ICC) of the per-record missingness
indicator with respect to entity membership.  High ICC means entire entities
tend to be missing together, creating covariate shift under entity-disjoint
train/test splits.
}
\usage{
mv_entity_correlation(d, text_cols, entity_col = "entity_id")
}
\arguments{
  \item{d}{Record dataset with an entity membership column.}
  \item{text_cols}{Character vector of column names.}
  \item{entity_col}{Name of the entity column. Default \code{"entity_id"}.}
}
\value{
A \code{data.table} with columns \code{column}, \code{icc} (0--1), and
\code{interpretation} (\code{"low (MCAR-like)"}, \code{"moderate (MAR-like)"},
or \code{"high (MNAR-like)"}).
}
\examples{
set.seed(2)
d <- data.frame(
  entity_id = rep(1:10, each = 5),
  name = c(rep(NA_character_, 10), paste0("n", 11:50))
)
mv_entity_correlation(d, "name")
}
