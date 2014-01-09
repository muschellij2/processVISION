\name{make.Date}
\alias{make.Date}
\title{Convert a vector to a date object}
\usage{
make.Date(x, format = "\%d-\%b-\%Y")
}
\arguments{
  \item{x}{vector of character or factor classes}

  \item{format}{date format to be passed to
  \code{\link{as.POSIXct}}, default to '\%d-\%b-\%Y'}
}
\value{
Date vector or vector of class passed in
}
\description{
Checks if all data can be converted to Date and forces any
"" to be NA
}
\examples{
x <- c("1", "2", "50.5")
make.Date(x)
x = c("01-Jan-2014", NA, "01-01-2014")
make.Date(x)
x = c("01-Jan-2014", NA)
make.Date(x)
}
\seealso{
\code{\link{write32.dta}}, \code{\link{as.Date}}
}

