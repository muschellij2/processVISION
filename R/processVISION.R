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
#' this will error (NULL)
#' @param drop.pattern vector of regular expression that will drop datasets 
#' that contain patterns using \link{grep}
#' @param keep.pattern vector of regular expression that will keep only datasets 
#' that contain any of the patterns \link{grep}
#' @param drop.new (logical) should records be dropped with 
#' a "New" formState (default TRUE)
#' @param fast (logical) if TRUE (default), will use \code{xmlToDF} 
#' (usually faster) to convert XML to data.frame.  
#' If FALSE, will use \code{xmlToDataFrame}
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
#' dsets, the dataset names, should match names(df.list), and 
#' runtimes, a list of \code{\link{system.time}} objects recorded how
#' long it took to convert to data.frame from XML.

processVISION <- function(xmlfile, 
                          isXML=FALSE,
                          drop.dsets = NULL, 
                          keep.dsets = NULL, 
                          dset.names = NULL, 
                          verbose=TRUE,
                          drop.pattern = NULL,
                          keep.pattern = NULL,
                          drop.new=TRUE,
                          fast = TRUE,
                          homogeneous = NA,
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
    dset.mat = matrix(dset.mat, ncol=length(drop.pattern))
    dset.drop = apply(dset.mat, 1, any)
    dsets <- dsets[ !dset.drop ]
  }
  
  ### keep pattern
  if (!is.null(keep.pattern)) {
    dset.mat = sapply(keep.pattern, grepl, x=dsets)
    dset.mat = matrix(dset.mat, ncol=length(keep.pattern))    
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
  
  runtimes = df.list
  ### loop through, converting every dataset to a data.frame.
  for (idset in 1:ndsets){
    dname <- dsets[idset]
    if (verbose) print(dname)
    dset <- paste0("/export_from_vision_EDC/", dname)
    
    #### get the records for that form
    nodeset <- getNodeSet(proc, dset)
    
    ### convert to data.frame - use xmlToDF or not
    if (fast) {
      runtime = system.time({
        dataset <- xmlToDF(doc=proc, xpath=dset)
        })
    } else {
      ### convert to data.frame
      runtime = system.time({
        dataset <- xmlToDataFrame(doc=proc, nodes=nodeset, 
                                homogeneous=homogeneous )
      })
    }
    if (verbose) {
      cat("Run Time for XML to DataFrame\n")
      print(runtime)
    }

    ### replace any empty strings with NA
    spaces <- sapply(dataset, function(x) x %in% "")
    dataset[spaces] <- NA
    
    if (drop.new) {
      dataset <- dataset[ !(dataset$formState %in% "New"), ]
    }
    
    df.list[[dname]] <- dataset
    runtimes[[dname]] = runtime
    
    if (writedta) create_stata_dta(df.list[dname], ...)
    
  }  
  
  
  ### get date/time from the export from the XML
  dt <- xpathApply(proc, "//export_from_vision_EDC", 
                   xmlGetAttr, "date")[[1]]
  ss <- strsplit(dt, " ")[[1]]
  datetime <- paste(ss[1:2], collapse="_")
  datetime <- gsub(":", "", datetime)
  
  return(list(df.list=df.list, datetime=datetime, 
              dsets=dsets, runtimes=runtimes))
  
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





#' Run a XML nodeset to Data frame
#' @name xmlToDF
#' @aliases xmlToDF
#' @description A different way of doing \code{\link{xmlToDataFrame}}
#' @param nodeset XMLNodeSet object (usually from \code{\link{getNodeSet}}
#' @param xpath XPath expression to extract the dataset 
#' @param usewhich (logical) use [which(logical),] versus [logical, ] for
#' subsetting
#' @param verbose (logical) for things to be printed (default = TRUE)
#' @export
#' @seealso \code{\link{xmlParse}}, \code{\link{xmlToDataFrame}}
#' @return A data.frame with the number of columns being the unique field
#' names from all nodes
xmlToDF = function(doc, xpath, usewhich = TRUE, verbose=TRUE, isXML = FALSE){
  
  if (isXML){
    ### make sure an XML document
    stopifnot(inherits(doc, "XMLAbstractDocument"))
    doc <- doc
  } else {
    doc <- xmlParse(doc)
  }  
  
  #### get the records for that form
  nodeset <- getNodeSet(doc, xpath)
  
  ## get the field names
  var.names <- lapply(nodeset, names)
  
  ## get the total fields that are in any record
  fields = unique(unlist(var.names))
  
  ## extract the values from all fields
  dl = lapply(fields, function(x) {
    if (verbose) print(paste0("  ", x))
    xpathSApply(proc, paste0(xpath, "/", x), xmlValue)
  })
  
  ## make logical matrix whether each record had that field
  name.mat = t(sapply(var.names, function(x) fields %in% x))
  df = data.frame(matrix(NA, nrow=nrow(name.mat), ncol=ncol(name.mat)))
  names(df) = fields
  
  ## fill in that data.frame
  for (icol in 1:ncol(name.mat)){
    rep.rows = name.mat[, icol]
    if (usewhich) rep.rows = which(rep.rows)
    df[rep.rows, icol] = dl[[icol]]
  }
  
  return(df)
}




