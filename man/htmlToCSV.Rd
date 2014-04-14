\name{htmlToCSV}
\alias{htmlToCSV}
\title{Read in a VISION html file with a table}
\usage{
htmlToCSV(file, writeFile = TRUE, outfile = NULL, tab.type = "queries",
  verbose = TRUE, colClasses = NULL)
}
\arguments{
  \item{file}{(character) to be read in}

  \item{writeFile}{(logical) Should a CSV be written}

  \item{outfile}{(character) filename of CSV to be written}

  \item{verbose}{(logical) diagnostic messages to be
  written}

  \item{colClasses}{(character) column classes for the
  table - needs to be same length and number of columns of
  table}
}
\value{
Data.frame of the table
}
\description{
This function reads in an HTML table from VISION and
returns a data.frame as well as writes a CSV for a more
manageable format rather than copying and pasting.
}

