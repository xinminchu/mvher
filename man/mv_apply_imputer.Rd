\name{mv_apply_imputer}
\alias{mv_apply_imputer}
\title{Apply a fitted imputer to a feature matrix}
\description{
Fills \code{NA} values in \code{X} using parameters learned by
\code{\link{mv_fit_imputer}}.  Apply the same fitted object to both training
and test matrices to avoid data leakage.
}
\usage{
mv_apply_imputer(X, imputer)
}
\arguments{
  \item{X}{A \code{data.frame} or \code{data.table} of numeric features.}
  \item{imputer}{An \code{mvher_imputer} object from \code{\link{mv_fit_imputer}}.}
}
\value{
A \code{data.table} with NAs replaced according to the fitted strategy.
}
\seealso{\code{\link{mv_fit_imputer}}, \code{\link{mv_impute}}}
\examples{
X  <- data.frame(a = c(0.8, NA, 0.3), b = c(NA, 0.6, 0.7))
Xt <- data.frame(a = c(NA, 0.5),      b = c(0.4, NA))
imp <- mv_fit_imputer(X, strategy = "mean")
mv_apply_imputer(X,  imp)
mv_apply_imputer(Xt, imp)
}
