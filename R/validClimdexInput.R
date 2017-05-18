## Check that climdexInput data structure is valid.
valid.climdexInput <- function(x) {
  temp.quantiles <- c(10, 90)
  prec.quantiles <- c(75, 95, 99)
  errors <- c()
  
  separate.base <- c(tmax=T, tmin=T, tavg=T, prec=F)
  present.data.vars <- names(x@data)
  length.check.slots <- c("dates", "jdays")
  length.check.members <- c("date.factors", "data")
  data.lengths <- c(sapply(x@data, length), sapply(length.check.slots, function(y) length(slot(x, y))), unlist(sapply(length.check.members, function(y) { sapply(slot(x, y), length) })))
  quantiles <- list(tmax=temp.quantiles, tmin=temp.quantiles, prec=prec.quantiles)
  
  if(!all(data.lengths == max(data.lengths)))
    errors <- c(errors, "Data fields, dates, and date factors must all be of the same length")
  
  ## Check that namasks have columns for each of the variables
  if(!all(c("annual", "halfyear", "seasonal", "monthly") %in% names(x@namasks)) || 
     !all(present.data.vars %in% names(x@namasks$annual) & present.data.vars %in% names(x@namasks$halfyear) &
          present.data.vars %in% names(x@namasks$seasonal) & present.data.vars %in% names(x@namasks$monthly)))
    errors <- c(errors, "NA mask for monthly, seasonal, halfyear and annual must contain data for all variables supplied.")
  
  ## Check that appropriate thresholds are present.
  need.base.data <- get.num.days.in.range(x@dates, x@base.range) > 0
  errors <- do.call(c, c(list(errors), lapply(intersect(present.data.vars, c("tmax", "tmin", "prec")), function(n) {
    if(is.null(quantiles[n]))
      return(NULL)
    ## FIXME: This test isn't necessarily valid and prevents calculating indices when no base period data is available.
    if(!(n %in% ls(envir=x@quantiles)))
      return(paste("Quantiles for", n, "are missing."))
    return(NULL)
  })))
  
  if(length(x@northern.hemisphere) != 1)
    errors <- c(errors, "northern.hemisphere must be of length 1.")
  
  if(length(errors) == 0)
    return(TRUE)
  else
    return(errors)
}

## Check that arguments to climdexInput.raw et al are complete enough and valid enough.
check.basic.argument.validity <- function(tmax, tmin, prec, 
                                          tmax.dates, tmin.dates, prec.dates, 
                                          base.range=c(1961, 1990), n=5, 
                                          tavg=NULL, tavg.dates=NULL) {
  
  check.var <- function(var, var.dates, var.name) {
    if(is.null(var) != is.null(var.dates))
      stop(paste("If passing in", var, ", must pass in", var, "dates too.."))
    if(!is.null(var.dates) && length(var) != length(var.dates))
      stop(paste("Length of", var.name, "data and dates do not match."))
    if(!is.null(var.dates) && !inherits(var.dates, "PCICt"))
      stop(paste(var.name, "dates must be of class PCICt."))
    if(!is.null(var) && !is.numeric(var))
      stop(paste(var.name, "must be of type numeric."))
  }
  
  check.var(tmax, tmax.dates, "tmax")
  check.var(tmin, tmin.dates, "tmin")
  check.var(tavg, tavg.dates, "tavg")
  check.var(prec, prec.dates, "prec")
  
  if(all(c(is.null(tmax), is.null(tmin), is.null(prec), is.null(tavg))))
    stop("Must supply at least one variable to calculate indices upon.")
  
  if(!(length(base.range) == 2 && is.numeric(base.range)))
    stop("Invalid base date range; expecting vector of 2 numeric years.")
  
  if(!is.numeric(n) || length(n) != 1)
    stop("n must be numeric and of length 1.")
  
  if(n != 5)
    warning("Use of n != 5 varies from the Climdex definition. Use at your own risk.")
}

## Check validity of quantile input.
check.quantile.validity <- function(quantiles, present.vars, days.in.base) {
  if(is.null(quantiles))
    return()
  
  if(class(quantiles) != "list")
    stop("Provided quantiles must be a list.")
  
  if(!all(present.vars %in% names(quantiles)))
    stop("Quantiles must be present for all variables provided.\n")
  
  if(!all(sapply(quantiles[names(quantiles) %in% intersect(present.vars, c("tmax", "tmin"))], function(x) { "outbase" %in% names(x) && all(c("q10", "q90") %in% names(x$outbase)) })))
    stop("Temperature out-of-base quantiles must contain 10th and 90th percentiles.\n")
  
  if(any(days.in.base > 0) && !all(sapply(quantiles[names(quantiles) %in% intersect(intersect(present.vars, c("tmax", "tmin")), names(days.in.base)[days.in.base > 0])], function(x) { "inbase" %in% names(x) && all(c("q10", "q90") %in% names(x$inbase)) })))
    stop("Temperature in-base quantiles must contain 10th and 90th percentiles.\n")
  
  if("prec" %in% names(quantiles) && !all(c("q95", "q99") %in% names(quantiles$prec)))
    stop("Precipitation quantiles must contain 95th and 99th percentiles.\n")
}


