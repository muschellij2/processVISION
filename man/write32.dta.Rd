\name{write32.dta}
\alias{my.write.dta}
\alias{write32.dta}
\alias{write32.dta,}
\title{Process a VISION XML file from their EDC}
\usage{
write32.dta(dataframe, file, version = 7L, convert.dates = TRUE,
  tz = "GMT", convert.factors = c("labels", "string", "numeric", "codes"),
  remove.nullstrings = TRUE)
}
\arguments{
  \item{dataframe}{a data frame.}

  \item{file}{character string giving filename.}

  \item{version}{integer: Stata version: 6, 7, 8 and 10 are
  supported, and 9 is mapped to 8, 11 to 10.}

  \item{convert.dates}{Convert Date and POSIXt objects to
  Stata dates?}

  \item{tz}{timezone for date conversion}

  \item{convert.factors}{how to handle factors}

  \item{remove.nullstrings}{logical (TRUE) Replace "" with
  NA}
}
\description{
Essentially a direct copy of \code{\link{write.dta}} from
the \code{foreign} package, except taking $<=$ 32 instead
of $<$ 32 characters in a variable name
}
\examples{
\dontrun{
}
}
\seealso{
\code{\link{write.dta}}
}

