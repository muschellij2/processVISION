\name{make.char}
\alias{make.char}
\title{Converts a character vector to no "" strings (changed to " ")}
\usage{
make.char(x, changeNA = TRUE)
}
\arguments{
  \item{x}{character vector}

  \item{changeNA}{change NA's to " " for characters}
}
\value{
Date vector or vector of class passed in
}
\description{
Checks if vector is character and then changes "" to " "
and has the option to change NA to " "
}
\examples{
x <- c(" ", "", NA, "hey", "blah", "2")
make.char(x)
make.char(x, changeNA=TRUE)
x = 1:10
make.Date(x)
}
\seealso{
\code{\link{write32.dta}}, \code{\link{as.character}}
}

