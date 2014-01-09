\name{make.numeric}
\alias{make.numeric}
\title{Convert a numeric vector if all data in vector is numeric}
\usage{
make.numeric(x)
}
\arguments{
  \item{x}{vector of data}
}
\value{
numeric vector or vector of class passed in
}
\description{
Checks if all data can be converted to numeric and forces
any "" to be NA
}
\examples{
x <- c("1", "2", "50.5")
make.numeric(x)
x[4] <- NA
make.numeric(x)
x[4] <- ""
make.numeric(x)
fac = factor(x)
make.numeric(x)
}
\seealso{
\code{\link{write32.dta}}, \code{\link{as.numeric}}
}

