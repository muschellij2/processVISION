#' @title Read in a VISION html file with a table
#' @description This function reads in an HTML table from VISION and returns
#' a data.frame as well as writes a CSV for a more manageable format rather than
#' copying and pasting.
#' @param File (character) to be read in
#' @param writeFile (logical) Should a CSV be written
#' @param outfile (character) filename of CSV to be written
#' @param verbose (logical) diagnostic messages to be written
#' @param colClasses (character) column classes for the table - needs to be
#' same length and number of columns of table
#' @return Data.frame of the table
htmlToCSV = function(file, 
                     writeFile = TRUE, 
                     outfile=NULL, 
                     tab.type = "queries",
                     verbose = TRUE, 
                     colClasses = NULL){
  ######################################
  # Outfile is the name (including .csv suffix) to be written
  ######################################
  if (is.null(outfile)){
    outdir = dirname(file)
    bn = basename(file)
    outfile = paste0(gsub("(.*)\\.htm", "\\1", bn), ".csv")
    outfile = file.path(outdir, outfile)
  }
  
  ######################################
  # Parse XML
  doc = htmlParse(file, useInternalNodes=TRUE)
  
  ######################################
  # Get table names from summary
  ######################################
  sums <- xpathSApply(doc, '//table', xmlGetAttr, "summary")
  sums = unlist(sums)
  if (verbose){
    cat("The types of tables available are:")
    cat(paste0(paste(sums, collapse=", "), "\n"))
  }
  
  ######################################
  # we want queries (SHINY will dynamically select this)
  ######################################
  which.sum = which(sums == tab.type)
  sum.chosen = sums[which.sum]
  xpath = paste0('//table[@summary="', sum.chosen , '"]')
  
  ######################################
  # Get nodes from this tables
  ######################################	
  nodeset <- getNodeSet(doc, xpath)
  xx = paste0(xpath, '//tr[@class="middle_row"]')
  hdrs = getNodeSet(doc, xx)
  
  ######################################
  # Get the colspan and rowspan info
  ######################################	
  hds = lapply(hdrs, function(x) xpathSApply(x, "./td", xmlValue))
  hd.l = lapply(hdrs, function(x) xpathSApply(x, "./td", 
                                              function(node) {
                                                cspan = xmlGetAttr(node, "colspan")
                                                if (is.null(cspan)) cspan = 1
                                                as.numeric(cspan)
                                              })
  )
  hd.rs = lapply(hdrs, function(x) xpathSApply(x, "./td", 
                                               function(node) {
                                                 cspan = xmlGetAttr(node, "rowspan")
                                                 if (is.null(cspan)) cspan = 1
                                                 as.numeric(cspan)
                                               })
  )
  nhds = length(hds)
  
  if (verbose){
    cat(paste0("Number of header rows:", nhds), "\n")
  }
  # hds = unlist(hds)
  # hd.l = unlist(hd.l)
  
  #####################################
  # Multiple spanning rows  - need to add info to the next line
  #####################################
  newhd = hds
  newhd.l = hd.l
  for (ihd in seq_along(hd.rs)){
    rs = hd.rs[[ihd]]
    len = hd.l[[ihd]]
    x = hds[[ihd]]
    lvec = rs > 1
    if (any(lvec)){
      ind = which(lvec)
      len = len[ind]
      x = x[ind]
      for (iind in seq_along(ind)){
        myind = ind[iind]
        putin = ""
        xx = newhd[[ihd + 1]]
        iseq = seq(0, myind -1)
        xx = c(xx[iseq], putin, xx[seq(myind, length(xx))])
        newhd[[ihd + 1]] = xx
        
        xx = newhd.l[[ihd + 1]]
        xx = c(xx[iseq], 1, xx[seq(myind, length(xx))])
        newhd.l[[ihd + 1]] = xx		
      }
    }
  }
  hds = newhd
  hd.l = newhd.l
  
  ######################################
  # Matrix of column heads 
  ######################################	
  hdr.mat = mapply(function(hd, nhd){
    rep(hd, nhd)
  }, hds, hd.l)
  hdr.mat = matrix(hdr.mat, ncol= nhds)
  hds = apply(hdr.mat, 1, function(x) {
    paste(str_trim(x), collapse = " ", sep = "")
  })
  
  if (verbose) {
    cat("Header names are:")
    cat(paste0(paste(hds, collapse=", "), "\n"))
  }
  # mynodes = nodeset[[1]]
  
  ######################################
  # Parse the table
  ######################################
  tables = readHTMLTable(nodeset[[1]], skip.rows = nhds, 
                         stringsAsFactors=FALSE)
  
  xtab = tables
  
  ######################################
  # Need to make sure this is a dataframe
  stopifnot(inherits(tables, "data.frame"))
  
  ######################################
  # Header workaround - readHTMLTable with header = TRUE didn't work
  ######################################
  tables = tables[-seq(1, nhds),]
  colnames(tables) = hds
  
  ######################################
  # Check column Classes
  ######################################
  if (!is.null(colClasses)){
    if (length(colClasses) != ncol(tables))	{
      stop(paste0("Length of colClasses (", length(colClasses),
                  "), not equal to ncol of tables (", ncol(tables), ")"))
    }
  }
  
  ######################################
  # Trim Whitespace and set classes
  ######################################	
  for (icol in seq(ncol(tables))){
    tables[, icol] = gsub(pattern="\n", replacement=" ",
                          tables[, icol])
    tables[, icol] = gsub("\\s+", " ",
                          tables[, icol])	
    tables[, icol] = str_trim(tables[, icol])
    if (!is.null(colClasses)){
      class(tables[, icol]) = colClasses[icol]
    }
  }
  
  ######################################
  # write out the csv
  ######################################	
  if (writeFile) {
    write.csv(tables, file = outfile, row.names=FALSE)
  }
  
  return(tables)
}
