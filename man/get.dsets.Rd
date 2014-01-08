\name{get.dsets}
\alias{get.dsets}
\title{Process a VISION XML file from their EDC}
\usage{
get.dnames(xml, isXML = FALSE, names.only = TRUE)
}
\arguments{
  \item{xml}{XML filename from VISION EDC, or if
  \code{\link{xmlParse}} has already been run, then the XML
  document with class \code{XMLAbstractDocument}. if
  (\code{isXML = FALSE}), should have ".xml" extension not
  ".zip".}

  \item{isXML}{(logical) indicating whether \code{xml} is
  an \code{XMLAbstractDocument} class (TRUE), or a filename
  (default FALSE)}

  \item{names.only}{(logical) return only the dataset
  names, (default TRUE)}
}
\value{
A list with slots dsets, the dataset names, and if
\code{names.only = FALSE}, proc, the XML parsed object.
}
\description{
Gets the dataset names from an VISION XML document
}
\examples{
\dontrun{
}
}
\seealso{
\code{\link{xmlParse}}, \code{\link{xmlRoot}}
}

