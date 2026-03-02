\name{mv_field_summary}
\alias{mv_field_summary}
\title{Summarise field-level missingness in a record dataset}
\description{
Counts empty strings and \code{NA}s per text column and reports the fraction
missing.  Useful as a first-pass diagnostic before choosing a missing-value
handling strategy.
}
\usage{
mv_field_summary(d, text_cols)
}
\arguments{
  \item{d}{A \code{data.frame} or \code{data.table} of records.}
  \item{text_cols}{Character vector of column names to inspect.}
}
\value{
A \code{data.table} with columns \code{column}, \code{n_total},
\code{n_empty}, and \code{pct_miss}.
}
\examples{
d <- data.frame(id = 1:4,
                name = c("Alice", "", NA, "Bob"),
                addr = c("", "", "X", "Y"))
mv_field_summary(d, c("name", "addr"))
}
