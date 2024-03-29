## Check that climdexInput data structure is valid.
valid.climdexInput <- function(x) {
  temp.quantiles <- c(10, 25, 75, 90)
  prec.quantiles <- c(25, 75, 95, 99)
  errors <- c()

  separate.base <- c(tmax=T, tmin=T, tavg=T, prec=F)
  present.data.vars <- names(x@data)
  length.check.slots <- c("dates", "jdays")
  length.check.members <- c("date.factors", "data")
  data.lengths <- c(sapply(x@data, length), sapply(length.check.slots, function(y) 
    length(slot(x, y))), unlist(sapply(length.check.members, function(y) { sapply(slot(x, y), length) })))
  quantiles <- list(tmax=temp.quantiles, tmin=temp.quantiles, prec=prec.quantiles)
  
  if(!all(data.lengths == max(data.lengths)))
    errors <- c(errors, "Data fields, dates, and date factors must all be of the same length")

  ## Check that namasks have columns for each of the variables
  if(!all(c("annual", "halfyear", "seasonal", "monthly") %in% names(x@namasks)) || 
     !all(present.data.vars %in% names(x@namasks$annual) & present.data.vars %in% names(x@namasks$halfyear) &
          present.data.vars %in% names(x@namasks$seasonal) & present.data.vars %in% names(x@namasks$monthly)))
    errors <- c(errors, "NA mask for monthly, seasonal, halfyear and annual must contain data for all variables supplied.")

  if(length(x@northern.hemisphere) != 1)
    errors <- c(errors, "northern.hemisphere must be of length 1.")
  
  if(length(errors) == 0)
    return(TRUE)
  else
    return(errors)
}

## Class definition declaration
#' climdexInput
#' 
#' The climdexInput class contains all the data necessary to compute the
#' climdex indices.
#' 
#' The \code{climdexInput} class consists of all the data necessary to compute
#' the climdex indices. Users will not need to modify any of the slots in this
#' class. That being said, users may want or need to repurpose this data for
#' further analysis. The following description of the data is aimed at that
#' audience.
#' 
#' The \code{data} slot contains time series' of daily data of equal length for
#' each of the provided variables. Missing days have been replaced with NA.
#' The \code{dates} slot is the corresponding series of dates (of type PCICt)
#' for the daily data.
#' 
#' The \code{quantiles} slot contains quantiles used for computing the
#' tn/tx 10/90p indices, w/csdi, r95ptot, and r99ptot. If precipitation data
#' is supplied, the 'prec' member contains the 95th and 99th percentile values
#' for precipitation within the base period. For tmin and tmax, if present each
#' will have a corresponding member in the slot. Within each of these, there
#' will be an 'inbase' and 'outbase' member, corresponding to thresholds to be
#' used within the base period (inbase) and outside the base period (outbase).
#' The 'inbase' member consists of one percentile for each day of the year,
#' computed using an n-day (default is 5-day) running window surrounding that
#' day. These percentiles are computed for at least the 10th and 90th
#' percentile of the data. For the 'outbase' member, given n years
#' of data to use as the base period, there are n * (n - 1) sets of daily
#' quantiles of the same type as those in 'inbase'.
#' 
#' To ease computation of monthly and annual data, \code{date.factors} 
#' contains date factors which group data into annual and monthly time
#' buckets. They are of the same length as the time series and can be reused
#' for computation of any annual or monthly aggregates.
#' 
#' The climdexInput class also includes NA masks for both monthly
#' and annual as parts of the \code{namasks} slot. Each of these masks consist
#' of a vector of numbers of the same length as the monthly or annual output
#' data. The values used are 1 to signify that the data meets the QC criteria,
#' and NA to signify it does not. Years with more than (by default) 15 days
#' missing, and months with more than (by default) 3 days missing, are
#' considered to be of poor quality and are masked here with NA. These
#' thresholds can be set when instantiating the object, and are stored in the
#' \code{max.missing.days} slot.
#' 
#' The \code{base.range} slot contains vector of type PCICt containing the
#' first and last day included in the baseline.
#' 
#' The \code{northern.hemisphere} slot contains a boolean indicating whether
#' the data came from the northern hemisphere. If FALSE, data is assumed to
#' have come from the southern hemisphere. This is used when computing growing
#' season length; if the data is from the southern hemisphere, growing season
#' length is the growing season starting in the beginning of July of the year
#' indicated, running to the end of June of the following year.
#' 
#' @name climdexInput
#' @aliases climdexInput-class
#' @docType class
#' @section Slots: \describe{
#' \item{data}{Time series of supplied data variables.}
#' \item{quantiles}{Threshold quantiles used for threshold-based indices.}
#' \item{namasks}{Data quality masks for annual and monthly data.}
#' \item{dates}{Date sequence (type PCICt) corresponding to temperature and
#' precipitation data.}
#' \item{jdays}{Julian days for the date sequence.}
#' \item{base.range}{Date range (type PCICt) of baseline period.}
#' \item{date.factors}{Factors used for creation of annual and monthly indices.}
#' \item{northern.hemisphere}{Boolean used when computing growing season
#' length.}
#' \item{max.missing.days}{Maximum number of missing days of data for annual
#' and monthly data.}
#' }
#' @seealso \code{\link{climdexInput.csv}}, \code{\link{climdexInput.raw}}.
#' @keywords climate ts
#' @examples
#' library(PCICt)
#' 
#' ## Parse the dates into PCICt.
#' tmax.dates <- as.PCICt(do.call(paste, ec.1018935.tmax[,c("year",
#' "jday")]), format="%Y %j", cal="gregorian")
#' tmin.dates <- as.PCICt(do.call(paste, ec.1018935.tmin[,c("year",
#' "jday")]), format="%Y %j", cal="gregorian")
#' prec.dates <- as.PCICt(do.call(paste, ec.1018935.prec[,c("year",
#' "jday")]), format="%Y %j", cal="gregorian")
#' 
#' ## Load the data in.
#' ci <- climdexInput.raw(ec.1018935.tmax$MAX_TEMP,
#' ec.1018935.tmin$MIN_TEMP, ec.1018935.prec$ONE_DAY_PRECIPITATION,
#' tmax.dates, tmin.dates, prec.dates, base.range=c(1971, 2000))
#' 
#' @export
setClass("climdexInput",
         representation(data = "list",
                        quantiles = "environment",
                        namasks = "list",
                        dates = "PCICt",
                        jdays = "numeric",
                        base.range = "PCICt",
                        date.factors = "list",
                        northern.hemisphere = "logical",
                        max.missing.days = "numeric"),
         validity=valid.climdexInput
         )

## Returns PCICt field or dies
get.date.field <- function(input.data, cal, date.types) {
  valid.date.types <- sapply(date.types, function(x) { return(!inherits(try(input.data[,x$fields], silent=TRUE), "try-error")) })

  if(sum(valid.date.types) == 0) {
    stop("Could not find a workable set of date fields")
  }

  date.type <- date.types[[which(valid.date.types)[1]]]
  date.strings <- do.call(paste, input.data[,date.type$fields])
  return(as.PCICt(date.strings, format=date.type$format, cal=cal))
}

## Creates a filled series given the data, dates, and new date sequence to be used.
create.filled.series <- function(data, data.dates, new.date.sequence) {
  new.data <- rep(NA, length(new.date.sequence))
  data.in.new.data <- (data.dates >= new.date.sequence[1]) & (data.dates <= new.date.sequence[length(new.date.sequence)])
  indices <- floor(as.numeric(data.dates[data.in.new.data] - new.date.sequence[1], units="days")) + 1
  new.data[indices] <- data[data.in.new.data]
  return(new.data)
}

## Check that arguments to climdexInput.raw et al are complete enough and valid enough.
check.basic.argument.validity <- function(tmax, tmin, prec, snow,  snow_new, wind, 
                                          wind_gust, wind_dir, cloud, sun, sun_rel,
                                          tmax.dates, tmin.dates, prec.dates, 
                                          snow.dates, snow_new.dates, wind.dates, 
                                          wind_gust.dates,
                                          wind_dir.dates, cloud.dates, sun.dates, 
                                          sun_rel.dates,base.range=c(1961, 1990), 
                                          n=5, tavg=NULL, tavg.dates=NULL) {
  
  
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
  check.var(snow, snow.dates, "snow")
  check.var(snow_new, snow_new.dates, "snow_new")
  check.var(wind, wind.dates, "wind")
  check.var(wind_gust, wind_gust.dates, "wind_gust")
  check.var(wind_dir, wind_dir.dates, "wind_dir")
  check.var(cloud, cloud.dates, "cloud")
  check.var(sun, sun.dates, "sun")
  check.var(sun_rel, sun_rel.dates, "sun_rel")
  
  if(all(c(is.null(tmax), is.null(tmin), is.null(tavg),
           is.null(prec),
           is.null(snow), is.null(snow_new),
           is.null(wind), is.null(wind_gust), is.null(wind_dir),
           is.null(cloud),
           is.null(sun),is.null(sun_rel))))
    stop("Must supply at least one variable to calculate indices upon.")
  
  if(!(length(base.range) == 2 && is.numeric(base.range)))
    stop("Invalid base date range; expecting vector of 2 numeric years.")
  
  if(!is.numeric(n) || length(n) != 1)
    stop("n must be numeric and of length 1.")
  
  if(n != 5)
    warning("Use of n != 5 varies from the Climdex definition. Use at your own risk.")
}


## Check validity of quantile input.
check.quantile.validity <- function(quantiles, present.vars, days.in.base, vars.require.quantiles) {
  if(is.null(quantiles))
    return()
  
  if (!is.null(quantiles)) {
    if(class(quantiles) != "list")
      stop("Provided quantiles must be a list.")
  }
  
  if(!all(intersect(present.vars, vars.require.quantiles) %in% names(quantiles)))
    stop("For temperature and precipitation, quantiles must be present for all variables provided.\n")

  if(!all(sapply(quantiles[names(quantiles) %in% intersect(present.vars, c("tmax", "tmin", "tavg"))], 
                 function(x) { "outbase" %in% names(x) && all(c("q10", "q25", "q75", "q90") %in% names(x$outbase)) })))
    stop("Temperature out-of-base quantiles must contain 10th, 25th, 75th and 90th percentiles.\n")

  if(any(days.in.base > 0) && !all(sapply(quantiles[names(quantiles) %in% intersect(intersect(present.vars, c("tmax", "tmin", "tavg")), 
                                                                                    names(days.in.base)[days.in.base > 0])], 
                                          function(x) { "inbase" %in% names(x) && all(c("q10", "q25", "q75", "q90") %in% names(x$inbase)) })))
    stop("Temperature in-base quantiles must contain 10th, 25th, 75th  and 90th percentiles.\n")

  if("prec" %in% names(quantiles) && !all(c("q25", "q75", "q95", "q99") %in% names(quantiles$prec)))
    stop("Precipitation quantiles must contain 25th, 75th, 95th and 99th percentiles.\n")
}

#' Method for creating climdexInput object from vectors of data
#' 
#' This function creates a climdexInput object from data already ingested into
#' R.
#' 
#' This function takes input climate data at daily resolution, and produces as
#' output a ClimdexInput data structure. This data structure can then be passed
#' to any of the routines used to compute the Climdex indices. The indices
#' themselves are specified on the webpage cited in the references section.
#' The \code{base.range} argument is a pair of 4 digit years which bound the
#' data on which the base percentiles are calculated.
#' 
#' The \code{tmax}, \code{tmin}, and \code{prec} arguments are numeric vectors
#' containing the data on which the indices are to be computed. The units are
#' assumed to be degrees C for temperature, and mm/day for precipitation.
#'
#' The \code{tmax.dates}, \code{tmin.dates}, and \code{prec.dates} arguments
#' are vectors of type \code{PCICt}.
#' 
#' The \code{n} argument specifies the size of the window used when computing
#' the percentiles used in \code{\link{climdex.tx10p}},
#' \code{\link{climdex.tn10p}}, \code{\link{climdex.tx90p}}, and
#' \code{\link{climdex.tn90p}}.
#' 
#' The \code{northern.hemisphere} argument specifies whether the data came from
#' the northern hemisphere. If FALSE, data is assumed to have come from the
#' southern hemisphere. This is used when computing growing season length; if
#' the data is from the southern hemisphere, growing season length is the
#' growing season starting in the beginning of July of the year indicated,
#' running to the end of June of the following year.
#' 
#' The \code{quantiles} argument allows the user to supply pre-computed quantiles.
#' This is a list consisting of quantiles for each variable.
#' 
#' For each temperature variable, there are separate lists of quantiles for 
#' inbase and outbase, with these names. In both cases, quantiles within these
#' lists are named q10 for the 10th percentile and q90 for the 90th percentile.
#' Other percentiles would be named qnn for the nnth percentile. For the
#' outbase quantiles, each element in the list is a vector of length 365 (or 360
#' in the case of 360-day calendars), corresponding to one value for each day of
#' the year. For the inbase quantiles, each element in the list is an array of
#' dimensions [365 or 360, nyr, nyr - 1], where nyr is the number of years in
#' the base period. Each value corresponds to a quantile for each day, for each
#' year, with a particular year replaced.
#'
#' For precipitation variables, there is a named vector of quantiles, consisting
#' of at least q95 and q99. 
#'
#' The \code{temp.qtiles} and \code{prec.qtiles} arguments allow the user to
#' modify the quantiles calculated. For example, specifying
#' temp.qtiles=c(0.10, 0.50, 0.90) would calculate the 10th, 50th, and 90th
#' percentiles for temperature.
#'
#' The \code{min.base.fraction.present} argument specifies the minimum fraction
#' of data which must be present for a quantile to be calculated for a 
#' particular day. If the fraction of data present is less than this threshold, 
#' the quantile for that day will be set to NA.
#'
#' The \code{max.missing.days} argument is a vector consisting of 'annual'
#' (the number of days that can be missing in a year) and 'monthly' (the
#' number of days that can be missing in a month. If one month in a year fails
#' the test, the corresponding year will be omitted.
#' 
#' @seealso \code{\link{climdex.pcic-package}}, \code{\link{strptime}}.
#' @references \url{http://etccdi.pacificclimate.org/list_27_indices.shtml}
#' @keywords ts climate
#'
#' @template climdexInput_raw_help1 
#' @template climdexInput_raw_params
#' @template climdexInput_common_params
#' @param northern.hemisphere Whether this point is in the northern hemisphere.
#' @param quantiles Threshold quantiles for supplied variables.
#' @param vars.require.quantiles which variables need to have quantiles present.
#' @param max.missing.days Vector containing thresholds for number of days
#' allowed missing per year (annual) and per month (monthly).
#' @return An object of class \code{\link{climdexInput-class}} for use with
#' other climdex methods.
#' @note Units are assumed to be mm/day for precipitation and degrees Celsius
#' for temperature. No units conversion is performed internally.
#' 
#' @export
climdexInput.raw <- function(tmax=NULL, tmax.dates=NULL,
                             tmin=NULL, tmin.dates=NULL,
                             tavg=NULL, tavg.dates=NULL,
                             prec=NULL, prec.dates=NULL,
                             snow=NULL, snow.dates=NULL,
                             snow_new=NULL, snow_new.dates=NULL,
                             wind=NULL, wind.dates=NULL,
                             wind_gust=NULL, wind_gust.dates=NULL,
                             wind_dir=NULL, wind_dir.dates=NULL,
                             cloud=NULL, cloud.dates=NULL,
                             sun=NULL, sun.dates=NULL,
                             sun_rel=NULL, sun_rel.dates=NULL,
                             quantiles=NULL, temp.qtiles=c(0.10, 0.25, 0.75, 0.90),
                             prec.qtiles=c(0.25, 0.75, 0.95, 0.99),
                             vars.require.quantiles = c('tmax', 'tmin', 'tavg', 'prec'),
                             base.range=c(1961, 1990), n=5, northern.hemisphere=TRUE,
                             max.missing.days=c(annual=15, halfyear=10, seasonal=8, monthly=3),
                             min.base.data.fraction.present=0.1) {
  
  ## Make sure all of these arguments are valid...
  check.basic.argument.validity(tmax=tmax, tmax.dates=tmax.dates,
                                tmin=tmin, tmin.dates=tmin.dates,
                                tavg=tavg, tavg.dates=tavg.dates,
                                prec=prec, prec.dates=prec.dates,
                                snow=snow, snow.dates=snow.dates,
                                snow_new=snow_new, snow_new.dates=snow_new.dates,
                                wind=wind, wind.dates=wind.dates,
                                wind_gust=wind_gust, wind_gust.dates=wind_gust.dates,
                                wind_dir=wind_dir, wind_dir.dates=wind_dir.dates,
                                cloud=cloud, cloud.dates=cloud.dates,
                                sun=sun, sun.dates=sun.dates,
                                sun_rel=sun_rel, sun_rel.dates=sun_rel.dates,
                                base.range=base.range,
                                n=n)

  stopifnot(length(max.missing.days) == 4 && all(c("annual", "halfyear", "seasonal", "monthly") %in% names(max.missing.days)))
  
  stopifnot(is.numeric(min.base.data.fraction.present) && length(min.base.data.fraction.present) == 1)
  
  d.list <- list(tmin.dates, tmax.dates, tavg.dates,
                 prec.dates,
                 snow.dates, snow_new.dates,
                 wind.dates, wind_gust.dates, wind_dir.dates,
                 cloud.dates,
                 sun.dates, sun_rel.dates)
  
  all.dates <- do.call(c, d.list[!sapply(d.list, is.null)])
  last.day.of.year <- get.last.monthday.of.year(all.dates)
  cal <- attr(all.dates, "cal")

  ## Convert base range (in years) to PCICt
  bs.date.range <- as.PCICt(paste(base.range, c("01-01", last.day.of.year), sep="-"), cal=cal)
  bs.date.series <- seq(bs.date.range[1], bs.date.range[2], by="day")
  
  ## Get dates for normal data
  new.date.range <- as.PCICt(paste(as.numeric(format(range(all.dates), "%Y", tz="GMT")), c("01-01", last.day.of.year), sep="-"), cal=cal)
  date.series <- seq(new.date.range[1], new.date.range[2], by="day")
  jdays <- get.jdays.replaced.feb29(get.jdays(date.series))
  
  ## Factors for dividing data up
  date.months <- as.numeric(format(date.series, format="%m", tz="GMT"))
  date.years  <- as.numeric(format(date.series, format="%Y", tz="GMT"))
  # get factors for seasons
  # Winter month D of prev year and JF of next year belong together, Year belongs to Jan => increase year of prev Dec by 1
  seas.years <- date.years
  seas.seas  <- date.months %/% 3 + 1
  seas.idx   <- which(seas.seas == 5)
  seas.years[seas.idx] <- seas.years[seas.idx]+1
  seas.seas[seas.idx]  <- 1
  # get factors for half years (winter (ONDJFM) & summer (APJJAS))
  # winter months OND of prev year and JFM of next year belong together, Year belongs to Jan => increase year of prev OND by 1
  half.years <- date.years
  half.half  <- (date.months+2) %/% 6 + 1
  half.idx   <- which(half.half == 3)
  half.years[half.idx] <- half.years[half.idx]+1
  half.half[half.idx]  <- 1
  
  # set up date.factors list
  date.factors <- list(annual=factor(format(date.series, format="%Y", tz="GMT")), 
                       halfyear=factor(paste(half.years,half.half,sep="-")),
                       seasonal=factor(paste(seas.years,seas.seas,sep="-")),
                       monthly=factor(format(date.series, format="%Y-%m", tz="GMT")))
  
  ## Filled data...
  var.list <- c("tmax", "tmin", "tavg", "prec", "snow", "snow_new", "wind", "wind_gust", "wind_dir",
                "cloud", "sun", "sun_rel")
  
  present.var.list <- var.list[sapply(var.list, function(x) !is.null(get(x)))]
  
  filled.list <- sapply(present.var.list, function(x) { 
    return(create.filled.series(get(x), trunc(get(paste(x, "dates", sep="."))), date.series)) }, simplify=FALSE)
  if(is.null(tavg) && !is.null(tmin) && !is.null(tmax))
    filled.list$tavg <- (filled.list$tmax + filled.list$tmin) / 2

  ## Establish some truth values for later use in logic...
  days.threshold <- 359
  present.dates <- sapply(present.var.list, function(x) get(paste(x, "dates", sep=".")))
  quantile.dates <- list(tmax=tmax.dates, tmin=tmin.dates, tavg=tavg.dates, prec=prec.dates)
  days.in.base <- sapply(quantile.dates, get.num.days.in.range, bs.date.range)

  ## Check that provided quantiles, if any, are valid
  ## if (!is.null(quantiles)) added by CH 2018
  if (!is.null(quantiles)) check.quantile.validity(quantiles, present.var.list, days.in.base, vars.require.quantiles)

  data.in.base.period <- any(days.in.base != 0)

  ## NA masks
  namasks <- list(annual=lapply(filled.list, get.na.mask, date.factors$annual, max.missing.days['annual']), 
                  halfyear=lapply(filled.list, get.na.mask, date.factors$halfyear, max.missing.days['halfyear']),
                  seasonal=lapply(filled.list, get.na.mask, date.factors$seasonal, max.missing.days['seasonal']),
                  monthly=lapply(filled.list, get.na.mask, date.factors$monthly, max.missing.days['monthly']))
  
  if (length(vars.require.quantiles) > 0) {
    if (is.null(quantiles)) {    # Quantiles required, but none passed. Calculate the quantiles on-the-fly.
      quantiles <- new.env(parent=emptyenv())    # Not assigning to list first as the call to as.environment will fail in that case
      if ('tmin' %in% vars.require.quantiles) delayedAssign("tmin", get.temp.var.quantiles(filled.list$tmin, date.series, bs.date.series, temp.qtiles, bs.date.range, n, TRUE, min.base.data.fraction.present), assign.env=quantiles)
      if ('tmax' %in% vars.require.quantiles) delayedAssign("tmax", get.temp.var.quantiles(filled.list$tmax, date.series, bs.date.series, temp.qtiles, bs.date.range, n, TRUE, min.base.data.fraction.present), assign.env=quantiles)
      if ('tavg' %in% vars.require.quantiles) delayedAssign("tavg", get.temp.var.quantiles(filled.list$tavg, date.series, bs.date.series, temp.qtiles, bs.date.range, n, TRUE, min.base.data.fraction.present), assign.env=quantiles)
      if ('prec' %in% vars.require.quantiles) delayedAssign("prec", get.prec.var.quantiles(filled.list$prec, date.series, bs.date.range, prec.qtiles), assign.env=quantiles)
    } else {
      quantiles <- as.environment(quantiles)
    }
  } else {
    quantiles = new.env()
  }
  
  return(new("climdexInput", data=filled.list, quantiles=quantiles, namasks=namasks, 
             dates=date.series, jdays=jdays, base.range=bs.date.range, date.factors=date.factors, 
             northern.hemisphere=northern.hemisphere, max.missing.days=max.missing.days))
}

#' Method for creating climdexInput object from CSV files
#' 
#' This function creates a climdexInput object from data in CSV files.
#' 
#' This function takes input climate data in CSV files at daily resolution,
#' and produces as output a ClimdexInput data structure. This data structure
#' can then be passed to any of the routines used to compute the Climdex
#' indices. The indices themselves are specified on the webpage cited in the
#' references section.
#'
#' Any of tmin.file (daily minimum temperature), tmax.file (daily maximum
#' temperature), tavg.file (daily mean temperature), and prec.file (daily
#' precipitation) can be passed in. tavg will be derived from the mean of
#' tmax and tmin if it is not supplied. If any of tmin.file, tmax.file, and
#' prec.file are not supplied, the set of indices which can be calculated will
#' be limited to indices which do not involve the missing variables.
#' 
#' The \code{tmax.file}, \code{tmin.file}, and \code{prec.file} arguments
#' should be names of CSV files containing dates and the data on which the
#' indices are to be computed. The units are assumed to be degrees C for
#' temperature, and mm/day for precipitation.
#' 
#' The \code{data.columns} argument is a vector consisting of named items tmax,
#' tmin, and prec. These named items are used as the column names in their
#' respective files when loading in CSV.
#' 
#' The \code{cal} argument is a textual description of the calendar type, as
#' described in the documentation for \code{\link{as.PCICt}}.
#' 
#' The \code{date.types} argument is a list of lists containing two named
#' items: \code{fields}, and \code{format}. The \code{fields} item is a vector
#' of names consisting of the columns to be concatenated together with spaces.
#' The \code{format} item is a date format as taken by \code{strptime}.
#'
#' For more details on arguments, see \code{link{climdexInput.raw}}.
#'
#' @seealso \code{\link{climdex.pcic-package}}, \code{\link{climdexInput.raw}}.
#' @references \url{http://etccdi.pacificclimate.org/list_27_indices.shtml}
#' @keywords ts climate
#' 
#' @param tmax.file Name of file containing daily maximum temperature data.
#' @param tmin.file Name of file containing daily minimum temperature data.
#' @param prec.file Name of file containing daily total precipitation data.
#' @param tavg.file Name of file containing daily mean temperature data.
#' @param snow.file Name of file containing daily mean snow height data.
#' @param snow_new.file Name of file containing daily mean new snow height data.
#' @param wind.file Name of file containing daily mean wind speed data.
#' @param wind_gust.file Name of file containing daily wind gust data.
#' @param wind_dir.file Name of file containing daily mean wind direction data.
#' @param cloud.file Name of file containing daily mean cloud cover data.
#' @param sun.file Name of file containing daily mean sunshine duration data.
#' @param sun_rel.file Name of file containing daily mean relative sunshine duration data.
#' @param data.columns Column names for tmin, tmax, and prec data.
#' @param date.types Column names for tmin, tmax, and prec data (see notes).
#' @param na.strings Strings used for NA values; passed to \link[utils]{read.table}
#' @param cal The calendar type used in the input files.
#' @template climdexInput_common_params
#' @param northern.hemisphere Whether this point is in the northern hemisphere.
#' @param quantiles Threshold quantiles for supplied variables.
#' @param max.missing.days Vector containing thresholds for number of days
#' allowed missing per year (annual) and per month (monthly).
#' @return An object of class \code{\link{climdexInput-class}} for use with
#' other climdex methods.
#' @note Units are assumed to be mm/day for precipitation and degrees Celsius
#' for temperature. No units conversion is performed internally.
#' @examples
#' ## This would create a climdexInput object from a set of filenames (already
#' ## stored as variables), with a different date format.
#' \dontrun{ci.csv <- climdexInput.csv(tmax.filename, tmin.filename,
#' prec.filename, date.types=list(list(fields=c("date"), format="%Y-%m-%d")))}
#'
#' @export
climdexInput.csv <- function(tmax.file=NULL, tmin.file=NULL, tavg.file=NULL, prec.file=NULL,
                             snow.file=NULL, snow_new.file=NULL,
                             wind.file=NULL, wind_gust.file=NULL, wind_dir.file=NULL,
                             cloud.file=NULL,
                             sun.file=NULL, sun_rel.file=NULL,
                             data.columns=list(tmin="tmin", tmax="tmax", tavg="tavg",
                                               prec="prec",
                                               snow="snow", snow_new="snow_new",
                                               wind="wind", wind_gust="wind_gust", wind_dir="wind_dir",
                                               cloud="cloud",
                                               sun="sun", sun_rel="sun_rel"),
                             base.range=c(1961, 1990),
                             na.strings=NULL, cal="gregorian",
                             date.types=NULL, n=5, northern.hemisphere=TRUE,
                             quantiles=NULL, temp.qtiles=c(0.10, 0.25, 0.75, 0.90), 
                             prec.qtiles=c(0.25, 0.75, 0.95, 0.99),
                             max.missing.days=c(annual=15, halfyear=10, seasonal=8, monthly=3),
                             min.base.data.fraction.present=0.1) {
  
  get.and.check.data <- function(fn, datacol) {
    if(!is.null(fn)){ 
      dat <- read.csv(fn, na.strings=na.strings)
      if(!(datacol %in% names(dat)))
        stop("Data column not found in tmin data.")
      return(list(dat=dat[!is.na(dat[,datacol]),datacol],  dates=get.date.field(dat, cal, date.types)))
    }
    return(list(dat=NULL, dates=NULL))
  }
  if(missing(date.types))
    date.types <- list(list(fields=c("year", "jday"), format="%Y %j"),
                       list(fields=c("year", "month", "day"), format="%Y %m %d"))
  else
    if(any(!sapply(date.types, function(x) { 
      return(sum(c("fields", "format") %in% names(x)) == 2 && is.character(x$fields) && is.character(x$format)) } )))
      stop("Invalid date.types specified. See ?climdexInput.csv .")

  tmin     <- get.and.check.data(tmin.file, data.columns$tmin)
  tmax     <- get.and.check.data(tmax.file, data.columns$tmax)
  tavg     <- get.and.check.data(tavg.file, data.columns$tavg)
  prec     <- get.and.check.data(prec.file, data.columns$prec)
  snow     <- get.and.check.data(snow.file, data.columns$snow)
  snow_new <- get.and.check.data(snow_new.file, data.columns$snow_new)
  wind     <- get.and.check.data(wind.file, data.columns$wind)
  wind_gust<- get.and.check.data(wind_gust.file, data.columns$wind_gust)
  wind_dir <- get.and.check.data(wind_dir.file, data.columns$wind_dir)
  cloud    <- get.and.check.data(cloud.file, data.columns$cloud)
  sun      <- get.and.check.data(sun.file, data.columns$sun)
  sun_rel  <- get.and.check.data(sun_rel.file, data.columns$sun_rel)
  
  return(climdexInput.raw(tmax=tmax$dat, tmax.dates=tmax$dates,
                          tmin=tmin$dat, tmin.dates=tmin$dates,
                          tavg=tavg$dat, tavg.dates=tavg$dates,
                          prec=prec$dat, prec.dates=prec$dates,
                          snow=snow$dat, snow.dates=snow$dates,
                          snow_new=snow_new$dat, snow_new.dates=snow_new$dates,
                          wind=wind$dat, wind.dates=wind$dates,
                          wind_gust=wind_gust$dat, wind_gust.dates=wind_gust$dates,
                          wind_dir=wind_dir$dat, wind_dir.dates=wind_dir$dates,
                          cloud=cloud$dat, cloud.dates=cloud$dates,
                          sun=sun$dat, sun.dates=sun$dates,
                          sun_rel=sun_rel$dat, sun_rel.dates=sun_rel$dates,
                          base.range=base.range, n=n, northern.hemisphere=northern.hemisphere,
                          quantiles=quantiles, temp.qtiles=temp.qtiles, prec.qtiles=prec.qtiles,
                          max.missing.days=max.missing.days, min.base.data.fraction.present=min.base.data.fraction.present))
}