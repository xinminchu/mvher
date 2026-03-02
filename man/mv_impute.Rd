\name{mv_impute}
\alias{mv_impute}
\title{Fit and apply imputation in one step}
\description{
Convenience wrapper: fits an imputer on \code{X_train} and applies it to both
\code{X_train} and (optionally) \code{X_test} in one call.
}
\usage{
mv_impute(X_train, X_test = NULL,
  strategy = c("mean", "median", "zero", "half", "hot_deck", "knn", "none"),
  k = 5L, seed = 1L)
}
\arguments{
  \item{X_train}{Training feature matrix.}
  \item{X_test}{Optional test feature matrix. Imputed using training parameters (no leakage).}
  \item{strategy}{Imputation strategy. See \code{\link{mv_fit_imputer}}.}
  \item{k}{Number of neighbours for \code{"knn"}. Default \code{5L}.}
  \item{seed}{Random seed. Default \code{1L}.}
}
\value{
A list with \code{X_train} (data.table), \code{X_test} (data.table or NULL),
and \code{imputer} (the fitted \code{mvher_imputer} object).
}
\seealso{\code{\link{mv_fit_imputer}}, \code{\link{mv_apply_imputer}}}
\examples{
Xtr <- data.frame(a = c(0.9, NA, 0.2), b = c(0.7, 0.8, NA))
Xte <- data.frame(a = c(NA, 0.5),      b = c(0.6, NA))
res <- mv_impute(Xtr, Xte, strategy = "median")
res$X_train
res$X_test
}
