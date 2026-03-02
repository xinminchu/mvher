\name{mv_fit_imputer}
\alias{mv_fit_imputer}
\title{Fit a feature-matrix imputer}
\description{
Learns imputation fill values from a training feature matrix.  The fitted
\code{mvher_imputer} object can then be applied to both training and test
matrices via \code{\link{mv_apply_imputer}}, avoiding data leakage.
}
\usage{
mv_fit_imputer(X,
  strategy = c("mean", "median", "zero", "half", "hot_deck", "knn", "none"),
  k = 5L,
  seed = 1L)
}
\arguments{
  \item{X}{A \code{data.frame} or \code{data.table} of numeric features.}
  \item{strategy}{Imputation strategy (see Details).}
  \item{k}{Number of neighbours for \code{"knn"}. Default \code{5L}.}
  \item{seed}{Random seed for \code{"hot_deck"} and \code{"knn"}. Default \code{1L}.}
}
\details{
\describe{
  \item{\code{"mean"}}{Fill NAs with column mean (default).}
  \item{\code{"median"}}{Fill NAs with column median.}
  \item{\code{"zero"}}{Fill NAs with 0.0.}
  \item{\code{"half"}}{Fill NAs with 0.5 (neutral similarity).}
  \item{\code{"hot_deck"}}{Random draw from non-missing observed training values.}
  \item{\code{"knn"}}{Average of k nearest complete training rows.}
  \item{\code{"none"}}{No imputation; NAs left as-is.}
}
}
\value{
An object of class \code{mvher_imputer}.
}
\seealso{\code{\link{mv_apply_imputer}}, \code{\link{mv_impute}}}
\examples{
X <- data.frame(a = c(0.8, NA, 0.3, 0.9), b = c(NA, 0.6, 0.7, 0.5))
imp <- mv_fit_imputer(X, strategy = "mean")
mv_apply_imputer(X, imp)
}
