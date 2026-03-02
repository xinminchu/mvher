\name{mv_pair_summary}
\alias{mv_pair_summary}
\title{Summarise missingness at the pair level}
\description{
For each candidate pair and each text column, reports how many pairs have at
least one empty field, both empty, or exactly one empty.
}
\usage{
mv_pair_summary(d, pairs, text_cols)
}
\arguments{
  \item{d}{Record dataset with an \code{id} column and all \code{text_cols}.}
  \item{pairs}{\code{data.frame}/\code{data.table} with columns \code{id1} and \code{id2}.}
  \item{text_cols}{Character vector of column names.}
}
\value{
A \code{data.table} with columns \code{column}, \code{type} (\code{"any_missing"},
\code{"both_missing"}, \code{"one_missing"}), \code{n_pairs}, \code{pct_pairs}.
}
\examples{
d <- data.frame(id = 1:4, name = c("Alice", "", NA, "Bob"))
pairs <- data.frame(id1 = c(1, 2, 3), id2 = c(2, 3, 4))
mv_pair_summary(d, pairs, "name")
}
