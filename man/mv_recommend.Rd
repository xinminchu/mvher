\name{mv_recommend}
\alias{mv_recommend}
\title{Recommend a missing-value handling strategy}
\description{
Uses simple decision rules based on overall missingness rate, entity-level
ICC, and the train/test split type to suggest the best field-comparison
method and imputer strategy.
}
\usage{
mv_recommend(field_miss_rate, entity_corr = NULL,
             has_entity_ids = TRUE,
             split_type = c("entity", "record"))
}
\arguments{
  \item{field_miss_rate}{Numeric scalar or vector. Overall fraction of records missing at least one text field (0--1).}
  \item{entity_corr}{Numeric scalar (0--1) or \code{NULL}. Entity-level ICC from \code{\link{mv_entity_correlation}}.}
  \item{has_entity_ids}{Logical. Whether entity labels are available at training time. Default \code{TRUE}.}
  \item{split_type}{\code{"entity"} (entity-disjoint split, default) or \code{"record"} (random).}
}
\value{
A list of class \code{mvher_recommendation} with elements
\code{field_method}, \code{imputer_strategy}, and \code{rationale}.
}
\seealso{\code{\link{mv_strategy_table}}, \code{\link{mv_entity_correlation}}}
\examples{
mv_recommend(field_miss_rate = 0.25, entity_corr = 0.6, split_type = "entity")
mv_recommend(field_miss_rate = 0.01)
}
