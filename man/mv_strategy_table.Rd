\name{mv_strategy_table}
\alias{mv_strategy_table}
\title{Summary table of all missing-value handling strategies}
\description{
Returns a \code{data.frame} describing every strategy supported by
\pkg{mvher} along dimensions useful for choosing among them.
}
\usage{
mv_strategy_table()
}
\value{
A \code{data.frame} with columns \code{strategy}, \code{type},
\code{handles_mcar}, \code{handles_mar}, \code{handles_mnar},
\code{adds_features}, \code{requires_labels}, \code{computation_cost},
\code{native_to_er}, \code{description}.
}
\seealso{\code{\link{mv_recommend}}}
\examples{
tbl <- mv_strategy_table()
tbl[tbl$type == "field-comparison", c("strategy", "handles_mnar")]
}
