\name{norm_str}
\alias{norm_str}
\title{Normalise a character vector for ER feature extraction}
\description{
Converts to lower-case, collapses internal whitespace, strips leading/trailing
whitespace, and replaces \code{NA} with \code{""}.
}
\usage{
norm_str(x)
}
\arguments{
  \item{x}{Character vector (or coercible to character).}
}
\value{
A character vector of the same length with no \code{NA}s.
}
\examples{
norm_str(c("  Hello World ", NA, "foo  bar"))
# [1] "hello world" ""          "foo bar"
}
