\name{xmlToDF}
\alias{xmlToDF}
\title{Run a XML nodeset to Data frame}
\usage{
xmlToDF(doc, xpath, usewhich = TRUE, verbose = TRUE)
}
\arguments{
  \item{nodeset}{XMLNodeSet object (usually from
  \code{\link{getNodeSet}}}

  \item{xpath}{XPath expression to extract the dataset}

  \item{usewhich}{(logical) use [which(logical),] versus
  [logical, ] for subsetting}

  \item{verbose}{(logical) for things to be printed
  (default = TRUE)}
}
\value{
A data.frame with the number of columns being the unique
field names from all nodes
}
\description{
A different way of doing \code{\link{xmlToDataFrame}}
}
\seealso{
\code{\link{xmlParse}}, \code{\link{xmlToDataFrame}}
}

