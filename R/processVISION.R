#' Process a VISION XML file from their EDC
#' @name processVISION
#' @aliases processVISION
#' @description This function takes in a XML and creates a list of 
#' R data.frames
#' @param xmlfile XML filename from VISION EDC, or if \code{\link{xmlParse}} 
#' has already been run, then the XML document with class 
#' \code{XMLAbstractDocument}.  
#' if (\code{isXML = FALSE}), should have ".xml" extension
#' not ".zip". 
#' @param isXML (logical) indicating whether \code{xml} is an 
#' \code{XMLAbstractDocument} class (TRUE), or a filename (default FALSE)
#' @param drop.dsets vector of dataset names that can be dropped (NULL)
#' @param keep.dsets vector of dataset names that are to be kept (NULL)
#' @param dset.names vector of dataset names to match (after dropping drop.dsets) 
#' if these do not match EXACTLY (case) the dataset names (all of them), 
#' this will error(NULL)
#' @param drop.pattern vector of regular expression that will drop datasets 
#' that contain patterns using \link{grep}
#' @param keep.pattern vector of regular expression that will keep only datasets 
#' that contain any of the patterns \link{grep}
#' @param drop.new (logical) should records be dropped with 
#' a "New" formState (default TRUE)
#' @param homogeneous (logical) should \code{\link{xmlToDataFrame}}
#' assume each node has all the variables (default TRUE), note this is 
#' different than default for xmlToDataFrame
#' @param writedta (logical) should the dta be written for each 
#' dataset? (default FALSE) 
#' @param ... arguments to be passed to \code{create_stata_dta}
#'    
#' @param verbose logical - Progress printed?(TRUE)
#' @export
#' @examples
#' \dontrun{
#' xmlfile = "MISTIE_III_Subjects_2014_01_09.xml"
#' df <- processVISION(xmlfile, isXML=FALSE, 
#' drop.pattern=c("^i_Doc_.*", "^docs_.*"),
#' drop.new=TRUE)
#' }
#' @seealso \code{\link{xmlParse}}, \code{\link{xmlRoot}}
#' @return A list with slots df.list, the list of datasets, 
#' datetime, the date/time the data was exported according to the XML,
#' dsets, the dataset names, should match names(df.list).

processVISION <- function(xmlfile, 
                          isXML=FALSE,
                          drop.dsets = NULL, 
                          keep.dsets = NULL, 
                          dset.names = NULL, 
                          verbose=TRUE,
                          drop.pattern = NULL,
                          keep.pattern = NULL,
                          drop.new=TRUE,
                          homogeneous = TRUE,
                          writedta = FALSE, ...){

  run <- get.dnames(xmlfile, isXML=isXML, names.only=FALSE)
  dsets <- run$dsets
  proc <- run$proc
  run <- NULL
  
  ### get the list of all data sets
  ldsets <- sort(unique(tolower(dsets)))
  
  ### dsets is the datasets to run
  if (!is.null(drop.dsets)) dsets <- dsets[ !(dsets %in% drop.dsets) ]
  if (!is.null(keep.dsets)) dsets <- dsets[  (dsets %in% keep.dsets) ]
  
  ### drop pattern
  if (!is.null(drop.pattern)) {
    dset.mat = sapply(drop.pattern, grepl, x=dsets)
    dset.drop = apply(dset.mat, 1, any)
    dsets <- dsets[ !dset.drop ]
  }
  
  ### keep pattern
  if (!is.null(keep.pattern)) {
    dset.mat = sapply(keep.pattern, grepl, x=dsets)
    dset.keep = apply(dset.mat, 1, any)
    dsets <- dsets[ dset.keep ]
  }
    
  ndsets <- length(dsets)
  
  sd.1 <- setdiff(dsets, dset.names)
  sd.2 <- setdiff(dset.names, dsets)
  
  if (length(sd.1) > 0 & !is.null(dset.names)){
    print(sd.1)
    stop("These are datasets in the xml, but not the check names")
  }
  if (length(sd.2) > 0){
    print(sd.2)
    stop("These are datasets in the checknames, but not the xml")
  }  
  
  df.list <- vector(mode="list", length=ndsets)
  names(df.list) <- dsets
  
  ### loop through, converting every dataset to a data.frame.
  for (idset in 1:ndsets){
    dname <- dsets[idset]
    if (verbose) print(dname)
    dset <- paste0("/export_from_vision_EDC/", dname)
    
    #### get the records for that form
    nodeset <- getNodeSet(proc, dset)
    
    ### convert to data.frame
    dataset <- xmlToDataFrame(doc=proc, nodes=nodeset, 
                              homogeneous=homogeneous )
    
    ### replace any empty strings with NA
    spaces <- sapply(dataset, function(x) x %in% "")
    dataset[spaces] <- NA
    
    if (drop.new) {
      dataset <- dataset[ !(dataset$formState %in% "New"), ]
    }
    
    df.list[[dname]] <- dataset
    
    if (writedta) create_stata_dta(df.list[dname], ...)
    
  }  
  
  
  ### get date/time from the export from the XML
  dt <- xpathApply(proc, "//export_from_vision_EDC", 
                   xmlGetAttr, "date")[[1]]
  ss <- strsplit(dt, " ")[[1]]
  datetime <- paste(ss[1:2], collapse="_")
  datetime <- gsub(":", "", datetime)
  
  return(list(df.list=df.list, datetime=datetime, dsets=dsets))
  
}



#' Process a VISION XML file from their EDC
#' @name get.dnames
#' @aliases get.dnames
#' @description Gets the dataset names from an VISION XML document
#' @param xml XML filename from VISION EDC, or if \code{\link{xmlParse}} 
#' has already been run, then the XML document with class 
#' \code{XMLAbstractDocument}.  
#' if (\code{isXML = FALSE}), should have ".xml" extension
#' not ".zip". 
#' @param isXML (logical) indicating whether \code{xml} is an 
#' \code{XMLAbstractDocument} class (TRUE), or a filename (default FALSE)
#' @param names.only (logical) return only the dataset names, (default TRUE)
#' @export
#' @examples
#' \dontrun{
#' xmlfile = "MISTIE_III_Subjects_2014_01_09.xml"
#' dnames <- get.dnames(xmlfile, isXML=FALSE, names.only=TRUE)$dsets
#' }
#' @seealso \code{\link{xmlParse}}, \code{\link{xmlRoot}}
#' @return A list with slots dsets, the dataset names, and if 
#' \code{names.only = FALSE}, proc, the XML parsed object.
get.dnames <- function(xml, isXML=FALSE, names.only=TRUE){
## parse the xml  
  if (isXML){
    ### make sure an XML document
    stopifnot(inherits(proc, "XMLAbstractDocument"))
    proc <- xml
  } else {
    proc <- xmlParse(xml)
  }
  ### get datasets name
  xroot <- xmlRoot(proc)
  n <- names(xroot)
  dsets <- unique(n)
  if (names.only){
    proc <- NULL
  }
  return(list(dsets=dsets, proc=proc))
}