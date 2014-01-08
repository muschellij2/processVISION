#' Process a VISION XML file from their EDC
#' @aliases write32.dta, my.write.dta
#' @description Essentially a direct copy of \code{\link{write.dta}} from
#' the \code{\link{foreign}} package, except taking <= 32 instead of 
#' < 32 characters in a variable name
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
