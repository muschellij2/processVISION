\name{make.char}
\alias{make.char}
\title{Converts a character vector to no "" strings (changed to another string)}
\usage{
make.char(x, changeNA = TRUE, change.char = " ")
}
\arguments{
  \item{x}{character vector}

  \item{changeNA}{change NA's to \code{change.char} for
  characters}

  \item{change.char}{(string) what to change "" to (default
  " ")}
}
\value{
Date vector or vector of class passed in
}
\description{
Checks if vector is character and then changes "" to a
different character string and has the option to change NA
to this string
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

