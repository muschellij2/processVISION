\name{processVISION}
\alias{processVISION}
\title{Process a VISION XML file from their EDC}
\usage{
processVISION(xmlfile, isXML = FALSE, drop.dsets = NULL,
  dset.names = NULL, verbose = TRUE, drop.pattern = NULL,
  keep.pattern = NULL, drop.new = TRUE)
}
\arguments{
  \item{xmlfile}{XML filename from VISION EDC, or if
  \code{\link{xmlParse}} has already been run, then the XML
  document with class \code{XMLAbstractDocument}. if
  (\code{isXML = FALSE}), should have ".xml" extension not
  ".zip".}

  \item{isXML}{(logical) indicating whether \code{xml} is
  an \code{XMLAbstractDocument} class (TRUE), or a filename
  (default FALSE)}

  \item{drop.dsets}{vector of dataset names that can be
  dropped (NULL)}

  \item{dset.names}{vector of dataset names to match (after
  dropping drop.dsets) if these do not match EXACTLY (case)
  the dataset names (all of them), this will error(NULL)}

  \item{drop.pattern}{vector of regular expression that
  will drop datasets that contain patterns using
  \link{grep}}

  \item{keep.pattern}{vector of regular expression that
  will keep only datasets that contain any of the patterns
  \link{grep}}

  \item{drop.new}{(logical) should records be dropped with
  a "New" formState (default TRUE)}

  \item{verbose}{logical - Progress printed?(TRUE)}
}
\value{
A list with slots df.list, the list of datasets, datetime,
the date/time the data was exported according to the XML,
dsets, the dataset names, should match names(df.list).
}
\description{
This function takes in a XML and creates a list of R
data.frames
}
\examples{
\dontrun{
}
}
\seealso{
\code{\link{xmlParse}}, \code{\link{xmlRoot}}
}

