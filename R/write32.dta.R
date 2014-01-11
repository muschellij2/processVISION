#' Process a VISION XML file from their EDC
#' @aliases write32.dta, my.write.dta
#' @description 
#' Essentially a direct copy of \code{\link{write.dta}} from
#' the \code{foreign} package, except taking $<=$ 32
#' instead of $<$ 32 characters in a variable name
#' @param dataframe a data frame.
#' @param file character string giving filename.
#' @param version	integer: Stata version: 6, 7, 8 and 10 are supported, and 9 is mapped to 8, 11 to 10.
#' @param convert.dates	Convert Date and POSIXt objects to Stata dates?
#' @param tz timezone for date conversion
#' @param convert.factors	how to handle factors
#' @param remove.nullstrings logical (TRUE) Replace "" with NA
#' @export
#' @examples
#' \dontrun{
#' }
#' @seealso \code{\link{write.dta}}
#' @return NULL

## changed the cutoff to 32 for namelength and not 31 ()
write32.dta <- function (dataframe, file, version = 7L, convert.dates = TRUE, 
    tz = "GMT", convert.factors = c("labels", "string", "numeric", 
        "codes"), remove.nullstrings=TRUE) 
{
    if (!is.data.frame(dataframe)) 
        stop("The object \"dataframe\" must have class data.frame")
    if (version < 6L) 
        stop("Version must be 6-12")
    if (version == 9L) 
        version <- 8L
    if (version == 11L) 
        version <- 10L
    if (version == 12L) 
        version <- 10L
    if (version > 12L) {
        warning("Version must be 6-12: using 7")
        version <- 7L
    }
    if (remove.nullstrings){
        spaces <- sapply(dataframe, function(x) x %in% "")
        dataframe[spaces] <- NA        
    }
    namelength <- if (version == 6L) 
        8L
    else 32L
    oldn <- names(dataframe)
    nn <- abbreviate(oldn, namelength)
    if (any(nchar(nn) > namelength)) 
        stop("cannot uniquely abbreviate variable names")
    if (any(nchar(oldn) > namelength)) 
        warning("abbreviating variable names")
    names(dataframe) <- nn
    attr(dataframe, "orig.names") <- oldn
    if (convert.dates) {
        dates <- which(sapply(dataframe, function(x) inherits(x, 
            "Date")))
        for (v in dates) dataframe[[v]] <- as.vector(julian(dataframe[[v]], 
            as.Date("1960-1-1", tz = "GMT")))
        dates <- which(sapply(dataframe, function(x) inherits(x, 
            "POSIXt")))
        for (v in dates) dataframe[[v]] <- as.vector(round(julian(dataframe[[v]], 
            ISOdate(1960, 1, 1, tz = tz))))
    }
    convert.factors <- match.arg(convert.factors)
    factors <- which(sapply(dataframe, is.factor))
    if (convert.factors == "string") {
        for (v in factors) dataframe[[v]] <- I(as.character(dataframe[[v]]))
    }
    else if (convert.factors == "numeric") {
        for (v in factors) dataframe[[v]] <- as.numeric(as.character(dataframe[[v]]))
    }
    else if (convert.factors == "codes") {
        for (v in factors) dataframe[[v]] <- as.numeric(dataframe[[v]])
    }
    shortlevels <- function(f) {
        ll <- levels(f)
        if (is.null(ll)) 
            return(NULL)
        if (all(nchar(ll, "bytes") <= 80L)) 
            ll
        else abbreviate(ll, 80L)
    }
    leveltable <- lapply(dataframe, shortlevels)
    if (any(sapply(dataframe, function(x) {
        d <- dim(x)
        !is.null(d) && d[1L] < length(x)
    }))) 
        stop("cannot handle multicolumn columns")
    invisible(.External(foreign:::do_writeStata, file, dataframe, version, 
        leveltable))
}

#environment(write32.dta) <- asNamespace('foreign')


#' Convert a numeric vector if all data in vector is numeric
#' @aliases make.numeric
#' @description Checks if all data can be converted to numeric and 
#' forces any "" to be NA
#' @param x vector of data
#' @export
#' @examples
#' x <- c("1", "2", "50.5")
#' make.numeric(x)
#' x[4] <- NA
#' make.numeric(x)
#' x[4] <- ""
#' make.numeric(x)
#' fac = factor(x)
#' make.numeric(x)
#' @seealso \code{\link{write32.dta}}, \code{\link{as.numeric}}
#' @return numeric vector or vector of class passed in
make.numeric <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  oldx <- x
  x[x == ""] <- NA
  if (all(is.na(x))) return(oldx)
  ## if they all are real - turn to numeric
  suppressWarnings(num_x <- as.numeric(x))
  keep <- !(is.na(x) & is.na(num_x))
  ### if all numeric 
  if (any(is.na(num_x[keep]))) {
    return(oldx)
  } else {
    return(num_x)
  }
}


#' Convert a vector to a date object
#' @aliases make.Date
#' @description Checks if all data can be converted to Date and 
#' forces any "" to be NA
#' @param x vector of character or factor classes
#' @param format date format to be passed to \code{\link{as.POSIXct}},
#' default to '\%d-\%b-\%Y'
#' @export
#' @examples
#' x <- c("1", "2", "50.5")
#' make.Date(x)
#' x = c("01-Jan-2014", NA, "01-01-2014")
#' make.Date(x)
#' x = c("01-Jan-2014", NA)
#' make.Date(x)
#' @seealso \code{\link{write32.dta}}, \code{\link{as.Date}}
#' @return Date vector or vector of class passed in
make.Date <- function(x, format='%d-%b-%Y') {
  if (is.numeric(x)) return(x)
  if (is.factor(x)) x <- as.character(x)
  oldx <- x
  qq <- (x == "")
  x[qq] <- NA
  if (all(is.na(x))) return(oldx)
  ## if date time string
  if (max(nchar(oldx)) > 11) return(oldx)            
  ## if they all are real - turn to numeric
  # suppressWarnings(date_x <- as.Date(x, format='%d-%b-%Y'))
  suppressWarnings(date_x <- as.POSIXct(x, format=format))
  keep <- !(is.na(x) & is.na(date_x))
  # date_x[qq] <- ""
  ### if all numeric 
  if (any(is.na(date_x[keep]))) {
    return(oldx)
  } else {
    return(date_x)
  }
}



#' Converts a character vector to no "" strings (changed to another string)
#' @aliases make.char
#' @description Checks if vector is character and then changes "" to a 
#' different character string
#' and has the option to change NA to this string
#' @param x character vector
#' @param changeNA change NA's to \code{change.char} for characters
#' @param change.char (string) what to change "" to (default " ")
#' @export
#' @examples
#' x <- c(" ", "", NA, "hey", "blah", "2")
#' make.char(x)
#' make.char(x, changeNA=TRUE)
#' x = 1:10
#' make.Date(x)
#' @seealso \code{\link{write32.dta}}, \code{\link{as.character}}
#' @return Date vector or vector of class passed in
#' 
make.char <- function(x, changeNA=TRUE, change.char=" ") {
  if (!("character" %in% class(x)) ) return(x)
  x[x %in% ""] <- change.char
  if (changeNA) x[is.na(x)] <- change.char
  return(x)
}
