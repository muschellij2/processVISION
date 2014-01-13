#' Process a list of data.frames from processVISION into .dta files for 
#' Stata
#' @aliases create_stata_dta
#' @description Processes a list of data.frames and uses 
#' \code{\link{write32.dta}} to create Stata data sets, converting factor 
#' and character vectors of each column of each dataset
#' @param df.list list of data frames from \code{\link{processVISION}}
#' @param outdir directory to put Stata .dta files (default ".")
#' @param date (character, default NULL) date of export, to be added to 
#' names of datasets
#' @param tryConvert (logical, default TRUE) should the function try to
#' make Date and numeric columns (TRUE) 
#' or just leave the data.frame as is (FALSE)?
#' @param lower.names (logical, default FALSE) should the column names for 
#' each variable be lowercase? 
#' @param version (integer, default 11L) version of Stata to create, 
#' passed to \code{\link{write.dta}}
#' @param convert.dates (logical, default TRUE) convert the dates to 
#' Stata dates? see \code{\link{write.dta}}
#' @param convert.factors (character, default "string") how to handle 
#' factors, see \code{\link{write.dta}}
#' @param verbose (logical, default TRUE) should names be printed for 
#' checking the progress?
#' @param trunc32 Truncate variables with > 32 characters?
#' @param ... Additional arguments to be passed to 
#' \code{\link{write32.dta}}
#' @export
#' @examples
#' \dontrun{
#' xmlfile = "MISTIE_III_Subjects_2014_01_09.xml"
#' df <- processVISION(xmlfile, isXML=FALSE, 
#' drop.pattern=c("^i_Doc_.*", "^docs_.*"),
#' drop.new=TRUE)
#' df.list <- df$df.list
#' check <- create_stata_dta(df.list, outdir=datadir)
#' }
#' @seealso \code{\link{write32.dta}}, \code{\link{write.dta}}, 
#' \code{\link{processVISION}}
#' @return data.frame with 2 columns: the dataset name and an indicator
#' if that dataset was converted.
create_stata_dta <- function(df.list, 
                             outdir=".",
                             date=NULL,
                             lower.names=FALSE, 
                             tryConvert = TRUE,
                             version=11L,
                             convert.dates=TRUE,
                             convert.factors="string",
                             verbose=TRUE,
                             trunc32 = FALSE,
                             ...){
  
  ndsets <- length(df.list)
  dsets <- names(df.list)
  
  idset <- 49
  
  check <- data.frame(dataset=dsets, stringsAsFactors=FALSE)
  check$converted = TRUE
  check$max.nchar = 0
  
  for (idset in seq_along(df.list)){

    xdname <- dname <- dsets[idset]
    if (verbose) print(xdname)
    
    dataset <- df.list[[idset]]
    
    ### getting maximum character lenght for variable names
    cn <- colnames(dataset)
    nc <- nchar(cn)
    check$max.nchar[idset] <- max(nc)
    cn <- tolower(cn)
    ## make sure no dup names
    stopifnot(!any(duplicated(cn)))
    if (lower.names) colnames(dataset) <- cn
    
    if (!trunc32) {
      stoppers = max(nc) > 32
      if (any(stoppers))
        print(xdname, " had over 32 character variable names")
        print(cn[stoppers])
        stop("can't convert with over 32")	
    }
    if (tryConvert){
      for (icol in 1:ncol(dataset)){
        colname <- cn[icol]
        if (is.factor(dataset[, icol])) 
          dataset[, icol] <- as.character(dataset[, icol])
        nchars <- nchar(dataset[, icol])
        dataset[, icol][nchars == 0] <- NA
        nas <- is.na(dataset[, icol])
        # dataset[nas, icol] <- ""
        dataset[, icol] <- as.character(dataset[, icol])
        if (grepl("date_time", colname)) {
          nas <- is.na(dataset[, icol])
          if (any(nas)) dataset[nas, icol] <- ""
          dataset[, icol] <- make.char(dataset[, icol])
          next;
        }
        dataset[, icol] <- make.numeric(dataset[, icol])
        # print(class(dataset[, icol]))
        dataset[, icol] <- make.Date(dataset[, icol])
        # print(class(dataset[, icol]))
        dataset[, icol] <- make.char(dataset[, icol])
        # print(class(dataset[, icol]))
      }
    }
    
    nchars <- sapply(dataset, function(x) {
      if (is.factor(x)) x <- as.character(x)
      if (!is.character(x)){
        return(0)
      } else {
        suppressWarnings( m <- max(nchar(x, "bytes") ))
        return(m  )
      }
    })
    
    ### self truncating the data
    # max.bytes <- 243
    # truncs <- which(nchars > max.bytes)
    # for (itrunc in seq_along(truncs)){
    # 	icol <- truncs[itrunc]
    # 	Encoding(dataset[, icol]) <- "bytes"
    # 	dataset[, icol] <- substr(dataset[, icol], 1, max.bytes)
    # }
    
    # print(max(nc))
    if (!is.null(date)) dname = paste0(dname, "_", date)
    dta <- file.path(outdir, paste0(dname, ".dta"))
    if (nrow(dataset) > 0) {
      write32.dta(dataset, file=dta, version=version,
                  convert.dates=convert.dates, 
                  convert.factors=convert.factors, ...
                  )
      
    } else {
      war <- paste0(xdname, " dataset had zero rows and was not converted")
      check$converted[idset] = FALSE
      warning(war)
    }
    # if (length(truncs) >0 ) stop("me")
    # if (any(nchars > 244)) stop("here")
    if (verbose) print(paste0(xdname, " Converted"))
  }
  
  return(check)
  
}