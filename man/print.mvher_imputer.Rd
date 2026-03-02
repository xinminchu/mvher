\name{print.mvher_imputer}
\alias{print.mvher_imputer}
\title{Print an mvher_imputer object}
\description{
Prints a short summary of an \code{mvher_imputer} object showing the
strategy and number of columns.
}
\usage{
\method{print}{mvher_imputer}(x, ...)
}
\arguments{
  \item{x}{An \code{mvher_imputer} object from \code{\link{mv_fit_imputer}}.}
  \item{...}{Unused.}
}
\value{
\code{x} invisibly.
}
\examples{
X <- data.frame(a = c(0.8, NA), b = c(NA, 0.6))
imp <- mv_fit_imputer(X, strategy = "knn")
print(imp)
}
