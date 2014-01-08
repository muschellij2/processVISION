

create_stata_dta <- function(df.list, datetime=NULL){
  
  ndsets <- length(df.list)
  dsets <- names(df.list)
  
  ncs <- rep(NA, length(dsets))
  
  for (idset in seq_along(dsets)){
    dname <- dsets[idset]
    dataset <- df.list[[idset]]
    
    ### getting maximum character lenght for variable names
    cn <- colnames(dataset)
    nc <- nchar(cn)
    ncs[idset] <- max(nc)
    cn <- tolower(cn)
    ## make sure no dup names
    stopifnot(!any(duplicated(cn)))
    colnames(dataset) <- cn
    
    stopifnot(max(nc) <= 32)	
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
        dataset[nas, icol] <- ""
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
    
    nchars <- sapply(dataset, function(x) {
      if (is.factor(x)) x <- as.character(x)
      if (!is.character(x)){
        return(0)
      } else {
        return( max(nchar(x, "bytes") ) )
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
    dta <- file.path(dtadir, paste0(dname, "_", date, ".dta"))
    my.write.dta(dataset, file=dta, version=11, 
                 convert.dates=TRUE, convert.factors=c("string") )
    # if (length(truncs) >0 ) stop("me")
    # if (any(nchars > 244)) stop("here")
    print(idset)
  }