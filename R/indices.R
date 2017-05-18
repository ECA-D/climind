#' Frost Days
#' 
#' This function computes the climdex index FD.
#' 
#' This function takes a climdexInput object as input and computes the FD (frost
#' days) climdex index: that is, the annual count of days where daily minimum
#' temperature drops below 0 degrees Celsius.
#' 
#' @param ci Object of type climdexInput.
#' @return A vector containing the number of frost days for each year.
#' @template generic_seealso_references
#' 
#' @templateVar cdxvar fd
#' @templateVar cdxdescription an annual timeseries of the number of frost days.
#' @template get_generic_example
#' 
#' @export
climdex.fd <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  stopifnot(!is.null(ci@data$tmin)); 
  return(number.days.op.threshold(ci@data$tmin, ci@date.factors[[match.arg(freq)]], 0, "<") * ci@namasks[[match.arg(freq)]]$tmin) }

#' Summer Days
#' 
#' This function computes the climdex index SU.
#' 
#' This function takes a climdexInput object as input and computes the SU (summer
#' days) climdex index: that is, the annual count of days where daily maximum
#' temperature exceeds 25 degrees Celsius.
#' 
#' @param ci Object of type climdexInput.
#' @return A vector containing the number of summer days for each year.
#' @template generic_seealso_references
#' @templateVar cdxvar su
#' @templateVar cdxdescription an annual timeseries of the number of summer days.
#' @template get_generic_example
#' 
#' @export
climdex.su <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  stopifnot(!is.null(ci@data$tmax)); 
  return(number.days.op.threshold(ci@data$tmax, ci@date.factors[[match.arg(freq)]], 25, ">") * ci@namasks[[match.arg(freq)]]$tmax) }

#' Icing Days
#' 
#' This function computes the climdex index ID.
#' 
#' This function takes a climdexInput object as input and computes the ID (icing
#' days) climdex index: that is, the annual count of days where daily maximum
#' temperature is below 0 degrees Celsius.
#' 
#' @param ci Object of type climdexInput.
#' @return A vector containing the number of icing days for each year.
#' @template generic_seealso_references
#' @templateVar cdxvar id
#' @templateVar cdxdescription an annual timeseries of the number of icing days.
#' @template get_generic_example
#' 
#' @export
climdex.id <- function(ci,  freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  stopifnot(!is.null(ci@data$tmax)); 
  return(number.days.op.threshold(ci@data$tmax, ci@date.factors[[match.arg(freq)]], 0, "<") * ci@namasks[[match.arg(freq)]]$tmax) }

#' Tropical Nights
#' 
#' This function computes the climdex index TR.
#' 
#' This function takes a climdexInput object as input and computes the TR
#' (tropical nights) climdex index: that is, the annual count of days where
#' daily minimum temperature stays above 20 degrees Celsius.
#' 
#' @param ci Object of type climdexInput.
#' @return A vector containing the number of frost days for each year.
#' @template generic_seealso_references
#' @templateVar cdxvar tr
#' @templateVar cdxdescription an annual timeseries of the number of tropical nights.
#' @template get_generic_example
#' 
#' @export
climdex.tr <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  stopifnot(!is.null(ci@data$tmin)); return(number.days.op.threshold(ci@data$tmin, ci@date.factors[[match.arg(freq)]], 20, ">") * ci@namasks[[match.arg(freq)]]$tmin) }

#' Growing Season Length
#' 
#' This function computes the growing season length (GSL) given the input.
#' 
#' This function takes a climdexInput object as input and computes the growing
#' season length based on this data.
#' 
#' Growing season length as defined by the climdex indices is the number of
#' days between the start of the first spell of warm days in the first half of
#' the year, and the start of the first spell of cold days in the second half
#' of the year. Spells of warm days are defined as six or more days with mean
#' temperature above 5 degrees Celsius; spells of cold days are defined as six
#' or more days with a mean temperature below 5 degrees Celsius.
#' 
#' The three alternate modes provided ('GSL_first', 'GSL_max', and 'GSL_sum')
#' are for testing purposes only. They differ considerably from the first
#' ('GSL') mode. All of them use a list of growing seasons -- here defined as
#' six or more consecutive days with a mean temperature greater than or equal
#' to 5 degrees Celsius, followed by either the end of the year or six or more
#' consecutive days with a mean temperature less than 5 degrees Celsius.
#' 'GSL_first' returns the first growing season found; 'GSL_max' returns the
#' longest growing season found; and 'GSL_sum' returns the total length of all
#' growing seasons found.
#' 
#' @param ci Object of type climdexInput.
#' @param gsl.mode Growing season length method to use.
#' @return A vector containing the number of days in the growing season for
#' each year.
#' @note Note that fclimdex results may differ from results using the first
#' ('GSL') mode due to bugs in fclimdex. Please ensure you are using the latest
#' version of fclimdex, as there have been numerous bug fixes and the results
#' should, at this point, match.
#' 
#' Please do not use the 'GSL_first', 'GSL_max', or 'GSL_sum' modes for
#' anything other than testing purposes at this time, nor should you rely on
#' this parameter being present in future versions of climdex.pcic.
#' @seealso \code{\link{growing.season.length}},
#' \code{\link{climdexInput.csv}}.
#' @references \url{http://etccdi.pacificclimate.org/list_27_indices.shtml}
#' @keywords ts climate
#' @templateVar cdxvar gsl
#' @templateVar cdxdescription an annual timeseries of the growing season length in days.
#' @template get_generic_example
#' 
#' @export
climdex.gsl <- function(ci, gsl.mode=c("GSL", "GSL_first", "GSL_max", "GSL_sum")) {
  stopifnot(!is.null(ci@data$tavg))
  ## Gotta shift dates so that July 1 is considered Jan 1 of same year in southern hemisphere
  if(ci@northern.hemisphere) {
    return(growing.season.length(ci@data$tavg, ci@date.factors$annual, ci@dates, ci@northern.hemisphere, gsl.mode=match.arg(gsl.mode)) * ci@namasks$annual$tavg)
  } else {
    dates.POSIXlt <- as.POSIXlt(ci@dates)
    years <- dates.POSIXlt$year + 1900
    months <- dates.POSIXlt$mon + 1
    
    valid.years <- range(years)
    years.gsl <- years - floor((12 - months) / 6)
    
    inset <- years.gsl >= valid.years[1]
    gsl.factor <- factor(years.gsl[inset])
    gsl.factor.monthly <- factor(paste(years.gsl[inset], months[inset], sep="-"))
    gsl.yearmonth.factor <- unlist(strsplit(levels(gsl.factor.monthly), "-"))[(0:(nlevels(gsl.factor.monthly) - 1)) * 2 + 1]
    gsl.temp.data <- ci@data$tavg[inset]
    namask.gsl.monthly <- get.na.mask(gsl.temp.data, gsl.factor.monthly, ci@max.missing.days['annual'])
    namask.gsl <- get.na.mask(gsl.temp.data, gsl.factor, ci@max.missing.days['annual']) * as.numeric(tapply(namask.gsl.monthly, gsl.yearmonth.factor, prod))
    dim(namask.gsl) <- dimnames(namask.gsl) <- NULL
    namask.gsl[length(namask.gsl)] <- NA
    return((growing.season.length(gsl.temp.data, gsl.factor, ci@dates[inset], ci@northern.hemisphere, gsl.mode=match.arg(gsl.mode)) * namask.gsl))
  }
}

#' Monthly Maximum of Daily Maximum Temperature
#'
#' This function computes the climdex index TXx.
#' 
#' This function takes a climdexInput object as input and computes
#' the monthly or annual maximum of daily maximum temperature.
#' 
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing the value of the index for each month.
#' @template generic_seealso_references
#' @templateVar cdxvar txx
#' @templateVar cdxdescription a monthly timeseries of maximum daily maximum temperature.
#' @template get_generic_example
#' 
#' @export
climdex.txx <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  stopifnot(!is.null(ci@data$tmax)); 
  return(suppressWarnings(tapply.fast(ci@data$tmax, ci@date.factors[[match.arg(freq)]], max, na.rm=TRUE)) * ci@namasks[[match.arg(freq)]]$tmax) }

#' Monthly Maximum of Daily Minimum Temperature
#'
#' This function computes the climdex index TNx.
#' 
#' This function takes a climdexInput object as input and computes
#' the monthly or annual maximum of daily minimum temperature.
#' 
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing the value of the index for each month.
#' @template generic_seealso_references
#' @templateVar cdxvar tnx
#' @templateVar cdxdescription a monthly timeseries of maximum daily minimum temperature.
#' @template get_generic_example
#' 
#' @export
climdex.tnx <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  stopifnot(!is.null(ci@data$tmin)); 
  return(suppressWarnings(tapply.fast(ci@data$tmin, ci@date.factors[[match.arg(freq)]], max, na.rm=TRUE)) * ci@namasks[[match.arg(freq)]]$tmin) }

#' Monthly Minimum of Daily Maximum Temperature
#'
#' This function computes the climdex index TXn.
#' 
#' This function takes a climdexInput object as input and computes
#' the monthly or annual minimum of daily maximum temperature.
#' 
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing the value of the index for each month.
#' @template generic_seealso_references
#' @templateVar cdxvar txn
#' @templateVar cdxdescription a monthly timeseries of minimum daily maximum temperature.
#' @template get_generic_example
#' 
#' @export
climdex.txn <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  stopifnot(!is.null(ci@data$tmax)); 
  return(suppressWarnings(tapply.fast(ci@data$tmax, ci@date.factors[[match.arg(freq)]], min, na.rm=TRUE)) * ci@namasks[[match.arg(freq)]]$tmax) }

#' Monthly Minimum of Daily Minimum Temperature
#'
#' This function computes the climdex index TNn.
#' 
#' This function takes a climdexInput object as input and computes
#' the monthly or annual minimum of daily minimum temperature.
#' 
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing the value of the index for each month.
#' @template generic_seealso_references
#' @templateVar cdxvar tnn
#' @templateVar cdxdescription a monthly timeseries of minimum daily minimum temperature.
#' @template get_generic_example
#' 
#' @export
climdex.tnn <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  stopifnot(!is.null(ci@data$tmin)); 
  return(suppressWarnings(tapply.fast(ci@data$tmin, ci@date.factors[[match.arg(freq)]], min, na.rm=TRUE)) * ci@namasks[[match.arg(freq)]]$tmin) }

## Our implementation currently follows the example set by fclimdex for dealing with missing values, which is wrong; it biases results upwards when missing values are present.

#' Percent of Values Below 10th Percentile Daily Minimum Temperature
#' 
#' This function computes the climdex index TN10p.
#' 
#' This function takes a climdexInput object as input and computes the
#' monthly or annual percent of values below the 10th percentile of baseline
#' daily minimum temperature.
#' 
#' @template threshold_indices_block
#' @template threshold_indices_args
#' @template missing_values_caveat
#'
#' @template generic_seealso_references
#' @templateVar cdxvar tn10p
#' @templateVar cdxdescription a monthly timeseries of the TN10p index.
#' @template get_generic_example
#' 
#' @export
climdex.tn10p <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  stopifnot(!is.null(ci@data$tmin) && !is.null(ci@quantiles$tmin)); 
  return(percent.days.op.threshold(ci@data$tmin, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], ci@quantiles$tmin$outbase$q10, ci@quantiles$tmin$inbase$q10, ci@base.range, "<", ci@max.missing.days[match.arg(freq)]) * ci@namasks[[match.arg(freq)]]$tmin) }

#' Percent of Values Below 10th Percentile Daily Maximum Temperature
#' 
#' This function computes the climdex index TX10p.
#' 
#' This function takes a climdexInput object as input and computes the
#' monthly or annual percent of values below the 10th percentile of baseline
#' daily maximum temperature.
#' 
#' @template threshold_indices_block
#' @template threshold_indices_args
#' @template missing_values_caveat
#'
#' @template generic_seealso_references
#' @templateVar cdxvar tx10p
#' @templateVar cdxdescription a monthly timeseries of the TX10p index.
#' @template get_generic_example
#' 
#' @export
climdex.tx10p <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax)); 
  return(percent.days.op.threshold(ci@data$tmax, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], ci@quantiles$tmax$outbase$q10, ci@quantiles$tmax$inbase$q10, ci@base.range, "<", ci@max.missing.days[match.arg(freq)]) * ci@namasks[[match.arg(freq)]]$tmax) }

#' Percent of Values Above 90th Percentile Daily Minimum Temperature
#' 
#' This function computes the climdex index TN90p.
#' 
#' This function takes a climdexInput object as input and computes the
#' monthly or annual percent of values above the 90th percentile of baseline
#' daily minimum temperature.
#' 
#' @template threshold_indices_block
#' @template threshold_indices_args
#' @template missing_values_caveat
#'
#' @template generic_seealso_references
#' @templateVar cdxvar tn90p
#' @templateVar cdxdescription a monthly timeseries of the TN90p index.
#' @template get_generic_example
#' 
#' @export
climdex.tn90p <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  stopifnot(!is.null(ci@data$tmin) && !is.null(ci@quantiles$tmin)); 
  return(percent.days.op.threshold(ci@data$tmin, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], ci@quantiles$tmin$outbase$q90, ci@quantiles$tmin$inbase$q90, ci@base.range, ">", ci@max.missing.days[match.arg(freq)]) * ci@namasks[[match.arg(freq)]]$tmin) }

#' Percent of Values Above 90th Percentile Daily Maximum Temperature
#' 
#' This function computes the climdex index TX90p.
#' 
#' This function takes a climdexInput object as input and computes the
#' monthly or annual percent of values above the 90th percentile of baseline
#' daily maximum temperature.
#' 
#' @template threshold_indices_block
#' @template threshold_indices_args
#' @template missing_values_caveat
#'
#' @template generic_seealso_references
#' @templateVar cdxvar tx90p
#' @templateVar cdxdescription a monthly timeseries of the TX90p index.
#' @template get_generic_example
#' 
#' @export
climdex.tx90p <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax)); 
  return(percent.days.op.threshold(ci@data$tmax, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], ci@quantiles$tmax$outbase$q90, ci@quantiles$tmax$inbase$q90, ci@base.range, ">", ci@max.missing.days[match.arg(freq)]) * ci@namasks[[match.arg(freq)]]$tmax) }

#' Warm Spell Duration Index
#'
#' This function computes the climdex index WSDI.
#' 
#' This function takes a climdexInput object as input and computes the climdex
#' index WSDI (Warm Spell Duration Index).
#' 
#' The warm spell duration index is defined as the number of days each year
#' which are part of a "warm spell". A "warm spell" is defined as a sequence of
#' 6 or more days in which the daily maximum temperature exceeds the 90th
#' percentile of daily maximum temperature for a 5-day running window
#' surrounding this day during the baseline period.
#' 
#' The \code{spells.can.span.years} option specifies whether spells can cross
#' year boundaries -- i.e., span years. The default for this is the same as
#' fclimdex.
#' 
#' @template wcsdi_common
#' @templateVar cdxvar wsdi
#' @templateVar cdxdescription an annual timeseries of the warm spell duration index.
#' @template get_generic_example
#' 
#' @export
climdex.wsdi <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal"), spells.can.span.years=FALSE) { 
  stopifnot(!is.null(ci@data$tmax) && !is.null(ci@quantiles$tmax)) 
  max_missing_days = ci@max.missing.days
  stopifnot(sum(match.arg(freq) == names(max_missing_days)) == 1)      # Check if we only get one TRUE, behavior is undefined if not
  current_max_missing_days = max_missing_days[match.arg(freq) == names(max_missing_days)]
  return(threshold.exceedance.duration.index(ci@data$tmax, ci@date.factors[[match.arg(freq)]], ci@jdays, ci@quantiles$tmax$outbase$q90, ">", spells.can.span.years=spells.can.span.years, max.missing.days=current_max_missing_days) * ci@namasks[[match.arg(freq)]]$tmax) }

#' Cold Spell Duration Index
#' 
#' This function computes the climdex index CSDI.
#' 
#' This function takes a climdexInput object as input and computes the climdex
#' index CSDI (Cold Spell Duration Index).
#'
#' The cold spell duration index is defined as the number of days
#' each year which are part of a "cold spell". A "cold spell" is defined as a
#' sequence of 6 or more days in which the daily minimum temperature is below
#' the 10th percentile of daily minimum temperature for a 5-day running window
#' surrounding this day during the baseline period.
#' 
#' The \code{spells.can.span.years} option specifies whether spells can cross
#' year boundaries -- i.e., span years. The default for this is the same as
#' fclimdex.
#' 
#' @template wcsdi_common
#' @templateVar cdxvar csdi
#' @templateVar cdxdescription an annual timeseries of the cold spell duration index.
#' @template get_generic_example
#' 
#' @export
climdex.csdi <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal"), spells.can.span.years=FALSE) { 
  stopifnot(!is.null(ci@data$tmin) && !is.null(ci@quantiles$tmin)); 
  max_missing_days = ci@max.missing.days
  stopifnot(sum(match.arg(freq) == names(max_missing_days)) == 1)      # Check if we only get one TRUE, behavior is undefined if not
  current_max_missing_days = max_missing_days[match.arg(freq) == names(max_missing_days)]
  return(threshold.exceedance.duration.index(ci@data$tmin, ci@date.factors[[match.arg(freq)]], ci@jdays, ci@quantiles$tmin$outbase$q10, "<", 
                                             spells.can.span.years=spells.can.span.years, max.missing.days=current_max_missing_days) * ci@namasks[[match.arg(freq)]]$tmin) }

#' Mean Diurnal Temperature Range
#' 
#' This function computes the diurnal temperature range on a monthly basis.
#' 
#' \code{climdex.dtr} computes the mean daily diurnal temperature range. The
#' frequency of observation can be either monthly or annual.
#' 
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @return A vector containing the mean monthly or mean annual diurnal
#' temperature range.
#' @note This function creates results which may differ in the 3rd decimal
#' place from the results from fclimdex.
#' @template generic_seealso_references
#' @templateVar cdxvar dtr
#' @templateVar cdxdescription a monthly timeseries of mean diurnal temperature range.
#' @template get_generic_example
#' 
#' @export
climdex.dtr <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  stopifnot(!is.null(ci@data$tmin) && !is.null(ci@data$tmax) && !is.null(ci@data$tavg)); 
  return(mean.daily.temp.range(ci@data$tmax, ci@data$tmin, ci@date.factors[[match.arg(freq)]]) * ci@namasks[[match.arg(freq)]]$tavg) }

#' Monthly Maximum 1-day Precipitation
#' 
#' This function computes the climdex index Rx1day.
#' 
#' This function takes a climdexInput object as input and computes the climdex
#' index Rx1day: monthly or annual maximum 1-day precipitation.
#' 
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @template rx5day_common
#' @template generic_seealso_references
#' @templateVar cdxvar rx1day
#' @templateVar cdxdescription a timeseries of monthly maximum 1-day precipitation.
#' @template get_generic_example
#' 
#' @export
climdex.rx1day <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  stopifnot(!is.null(ci@data$prec)); 
  return(nday.consec.prec.max(ci@data$prec, ci@date.factors[[match.arg(freq)]], 1) * ci@namasks[[match.arg(freq)]]$prec) }

#' Monthly Maximum 5-day Consecutive Precipitation
#' 
#' This function computes the climdex index Rx5day.
#' 
#' This function takes a climdexInput object as input and computes the climdex
#' index Rx5day: monthly or annual maximum 5-day consecutive precipitation.
#' 
#' @param ci Object of type climdexInput.
#' @param freq Time frequency to aggregate to.
#' @param center.mean.on.last.day Whether to center the 5-day running mean on
#' the last day of the window, instead of the center day.
#' @template rx5day_common
#' @template generic_seealso_references
#' @templateVar cdxvar rx5day
#' @templateVar cdxdescription a timeseries of monthly maximum 5-day consecutive precipitation.
#' @template get_generic_example
#' 
#' @export
climdex.rx5day <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal"), center.mean.on.last.day=FALSE) { 
  stopifnot(!is.null(ci@data$prec)); 
  return(nday.consec.prec.max(ci@data$prec, ci@date.factors[[match.arg(freq)]], 5, center.mean.on.last.day) * ci@namasks[[match.arg(freq)]]$prec) }

#' Simple Precpitation Intensity Index
#' 
#' This function computes the climdex index SDII.
#' 
#' \code{climdex.sdii} computes the climdex index SDII, or Simple Precipitation
#' Intensity Index. This is defined as the sum of precipitation in wet days
#' (days with preciptitation over 1mm) during the year divided by the number of
#' wet days in the year.
#' 
#' @param ci Object of type climdexInput.
#' @return A vector containing the value of the index for each year.
#' @note fclimdex rounds to 1 decimal place, whereas climdex.sdii does not.
#' This results in some small differences.
#' @template generic_seealso_references
#' @templateVar cdxvar sdii
#' @templateVar cdxdescription a timeseries of annual SDII values.
#' @template get_generic_example
#' 
#' @export
climdex.sdii <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  stopifnot(!is.null(ci@data$prec)); 
  return(simple.precipitation.intensity.index(ci@data$prec, ci@date.factors[[match.arg(freq)]]) * ci@namasks[[match.arg(freq)]]$prec) }

#' Precipitation Exceeding 10mm Per Day
#' 
#' This function computes the climdex index R10mm.
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index R10mm: the annual count of days where daily precipitation is more than 10mm per day.
#' 
#' @param ci Object of type climdexInput.
#' @return A vector containing the value of the index for each year.
#' @template generic_seealso_references
#' @templateVar cdxvar r10mm
#' @templateVar cdxdescription an annual timeseries of the R10mm index.
#' @template get_generic_example
#' 
#' @export
climdex.r10mm <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) {
  stopifnot(!is.null(ci@data$prec)); 
  return(number.days.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], 10, ">=") * ci@namasks[[match.arg(freq)]]$prec) }

#' Precipitation Exceeding 20mm Per Day
#' 
#' This function computes the climdex index R20mm.
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index R20mm: the annual count of days where daily precipitation is more than 20mm per day.
#' 
#' @param ci Object of type climdexInput.
#' @return A vector containing the value of the index for each year.
#' @template generic_seealso_references
#' @templateVar cdxvar r20mm
#' @templateVar cdxdescription an annual timeseries of the R20mm index.
#' @template get_generic_example
#' 
#' @export
climdex.r20mm <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  stopifnot(!is.null(ci@data$prec)); 
  return(number.days.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], 20, ">=") * ci@namasks[[match.arg(freq)]]$prec) }

#' Precipitation Exceeding A Specified Amount Per Day
#' 
#' This function computes the climdex index Rnnmm.
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index Rnnmm: the annual count of days where daily precipitation is more than \code{nn} mm per day.
#' 
#' @param ci Object of type climdexInput.
#' @param threshold The threshold to be used for Rnnmm.
#' @return A vector containing the value of the index for each year.
#' @template generic_seealso_references
#' @templateVar cdxvar rnnmm
#' @templateVar cdxdescription an annual timeseries of the R1mm index.
#' @template get_generic_example
#' 
#' @export
climdex.rnnmm <- function(ci, threshold=1, freq=c("monthly", "annual", "halfyear", "seasonal")) {
  stopifnot(!is.null(ci@data$prec));
  if(!is.numeric(threshold) || length(threshold) != 1) stop("Please specify a single numeric threshold value.");
  
  return(number.days.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], threshold, ">=") * ci@namasks[[match.arg(freq)]]$prec)
}

#' Maximum Consecutive Dry Days
#' 
#' This function computes the climdex index CDD.
#'
#' This function computes the climdex index CDD: the annual maximum length of dry spells, in days.
#' Dry spells are considered to be sequences of days where daily preciptation
#' is less than 1mm per day.
#' 
#' @template cdd_common
#' @templateVar cdxvar cdd
#' @templateVar cdxdescription an annual timeseries of the CDD index.
#' @template get_generic_example
#' 
#' @export
climdex.cdd <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal"), spells.can.span.years=TRUE) { 
  stopifnot(!is.null(ci@data$prec)); 
  return(spell.length.max(ci@data$prec, ci@date.factors[[match.arg(freq)]], 1, "<", spells.can.span.years) * ci@namasks[[match.arg(freq)]]$prec) }

#' Maximum Consecutive Wet Days
#' 
#' This function computes the climdex index CWD.
#' 
#' This function takes a climdexInput object as input and computes the climdex
#' index CWD: the annual maximum length of wet spells, in days.
#' Wet spells are considered to be sequences of days where daily precipitation
#' is at least 1mm per day.
#' 
#' @template cdd_common
#' @templateVar cdxvar cdd
#' @templateVar cdxdescription an annual timeseries of the CWD index.
#' @template get_generic_example
#' 
#' @export
climdex.cwd <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal"), spells.can.span.years=TRUE) { 
  stopifnot(!is.null(ci@data$prec)); 
  return(spell.length.max(ci@data$prec, ci@date.factors[[match.arg(freq)]], 1, ">=", spells.can.span.years) * ci@namasks[[match.arg(freq)]]$prec) }

#' Total Daily Precipitation Exceeding 75\%ile Threshold
#' 
#' This function computes the climdex index R75p.
#' 
#' This function takes a climdexInput object as input and computes the climdex
#' index R75p: the annual sum of precipitation in days where daily precipitation exceeds the
#' 75th percentile of daily precipitation in the base period.
#' 
#' @param ci Object of type climdexInput.
#' @return A vector containing an annual timeseries of precipitation exceeding
#' the threshold.
#' @template generic_seealso_references
#' @templateVar cdxvar r75p
#' @templateVar cdxdescription an annual timeseries of the R75p index.
#' @template get_generic_example
#' 
#' @export
climdex.r75p <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  stopifnot(!is.null(ci@data$prec) && !is.null(ci@quantiles$prec)); 
  return(total.precip.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], ci@quantiles$prec['q75'], ">") * ci@namasks[[match.arg(freq)]]$prec) }


#' Total Daily Precipitation Exceeding 95\%ile Threshold
#' 
#' This function computes the climdex index R95p.
#' 
#' This function takes a climdexInput object as input and computes the climdex
#' index R95p: the annual sum of precipitation in days where daily precipitation exceeds the
#' 95th percentile of daily precipitation in the base period.
#' 
#' @param ci Object of type climdexInput.
#' @return A vector containing an annual timeseries of precipitation exceeding
#' the threshold.
#' @template generic_seealso_references
#' @templateVar cdxvar r95p
#' @templateVar cdxdescription an annual timeseries of the R95p index.
#' @template get_generic_example
#' 
#' @export
climdex.r95p <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  stopifnot(!is.null(ci@data$prec) && !is.null(ci@quantiles$prec)); 
  return(total.precip.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], ci@quantiles$prec['q95'], ">") * ci@namasks[[match.arg(freq)]]$prec) }

#' Total Daily Precipitation Exceeding 99\%ile Threshold
#' 
#' This function computes the climdex index R99p.
#' 
#' This function takes a climdexInput object as input and computes the climdex
#' index R99p: the annual sum of precipitation in days where daily precipitation exceeds the
#' 99th percentile of daily precipitation in the base period.
#' 
#' @param ci Object of type climdexInput.
#' @return A vector containing an annual timeseries of precipitation exceeding
#' the threshold.
#' @template generic_seealso_references
#' @templateVar cdxvar r99p
#' @templateVar cdxdescription an annual timeseries of the R99p index.
#' @template get_generic_example
#' 
#' @export
climdex.r99p <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  stopifnot(!is.null(ci@data$prec) && !is.null(ci@quantiles$prec));
  return(total.precip.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], ci@quantiles$prec['q99'], ">") * ci@namasks[[match.arg(freq)]]$prec) }

#' Precipitation fraction due to moderate wet days (exceeding 75\%ile Threshold)
#' 
#' This function computes the climdex index R75pTOT.
#' 
#' This function takes a climdexInput object as input and computes the climdex
#' index R75pTOT: the fraction of the sum of precipitation in days where daily precipitation exceeds the
#' 75th percentile of daily precipitation in the base period.
#' 
#' @param ci Object of type climdexInput.
#' @return A vector containing an annual timeseries of precipitation exceeding
#' the threshold.
#' @template generic_seealso_references
#' @templateVar cdxvar r75ptot
#' @templateVar cdxdescription an annual timeseries of the R75pTOT index.
#' @template get_generic_example
#' 
#' @export
climdex.r75ptot <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  
  stopifnot(!is.null(ci@data$prec),!is.null(ci@quantiles$prec)); 
  prcptot <- total.precip.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], 1, ">=") * ci@namasks[[match.arg(freq)]]$prec
  r75p <- total.precip.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], ci@quantiles$prec['q75'], ">") * ci@namasks[[match.arg(freq)]]$prec
  return(100*r75p/prcptot) }

#' Precipitation fraction due to very wet days (exceeding 95\%ile Threshold)
#' 
#' This function computes the climdex index R95pTOT.
#' 
#' This function takes a climdexInput object as input and computes the climdex
#' index R95pTOT: the fraction of the sum of precipitation in days where daily precipitation exceeds the
#' 95th percentile of daily precipitation in the base period.
#' 
#' @param ci Object of type climdexInput.
#' @return A vector containing an annual timeseries of precipitation exceeding
#' the threshold.
#' @template generic_seealso_references
#' @templateVar cdxvar r95ptot
#' @templateVar cdxdescription an annual timeseries of the R95pTOT index.
#' @template get_generic_example
#' 
#' @export
climdex.r95ptot <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  
  stopifnot(!is.null(ci@data$prec),!is.null(ci@quantiles$prec)); 
  prcptot <- total.precip.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], 1, ">=") * ci@namasks[[match.arg(freq)]]$prec
  r95p <- total.precip.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], ci@quantiles$prec['q95'], ">") * ci@namasks[[match.arg(freq)]]$prec
  return(100*r95p/prcptot) }

#' Precipitation fraction due to extremly wet days (exceeding 99\%ile Threshold)
#' 
#' This function computes the climdex index R99pTOT.
#' 
#' This function takes a climdexInput object as input and computes the climdex
#' index R99pTOT: the fraction of the sum of precipitation in days where daily precipitation exceeds the
#' 99th percentile of daily precipitation in the base period.
#' 
#' @param ci Object of type climdexInput.
#' @return A vector containing an annual timeseries of precipitation exceeding
#' the threshold.
#' @template generic_seealso_references
#' @templateVar cdxvar r99ptot
#' @templateVar cdxdescription an annual timeseries of the R99pTOT index.
#' @template get_generic_example
#' 
#' @export
climdex.r99ptot <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  
  stopifnot(!is.null(ci@data$prec),!is.null(ci@quantiles$prec)); 
  prcptot <- total.precip.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], 1, ">=") * ci@namasks[[match.arg(freq)]]$prec
  r99p <- total.precip.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], ci@quantiles$prec['q99'], ">") * ci@namasks[[match.arg(freq)]]$prec
  return(100*r99p/prcptot) }

#' Total Daily Precipitation
#' 
#' This function computes the climdex index PRCPTOT.
#' 
#' This function takes a climdexInput object as input and computes the climdex
#' index PRCPTOT: the annual sum of precipitation in wet days
#' (days where precipitation is at least 1mm).
#' 
#' @param ci Object of type climdexInput.
#' @return A vector containing an annual timeseries of precipitation in wet days.
#' @template generic_seealso_references
#' @templateVar cdxvar prcptot
#' @templateVar cdxdescription an annual timeseries of the sum of precipitation in wet days.
#' @template get_generic_example
#' 
#' @export
climdex.prcptot <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal")) { 
  stopifnot(!is.null(ci@data$prec)); 
  return(total.precip.op.threshold(ci@data$prec, ci@date.factors[[match.arg(freq)]], 1, ">=") * ci@namasks[[match.arg(freq)]]$prec) }

#' Standardized Precipitation Index 3-mon
#' @description Adapted by ClimPACT2 and SPEI (arguments taken from here) libraries.
#' Given a timeseries of daily precipitation amounts [mm], the function calculates the Standardized Precipitation Index (SPI) as it is created and defined by the SPEI library.
#' This function computes the climdex index spi at multiple scales.
#' 
#' @param ci Object of type climdexInput (representing the daily precipitation [mm])
#' @param freq Time frequency to aggregate to. Allowed only monthly
#' @param scale an integer, representing the time scale at which the SPI will be computed. Default is 3
#' @param distribution name of the distribution function to be used for computing the SPI (default ’Gamma’)
#' @param fit name of the method used for computing the distribution function parameters (default 'ub-pwm')
#' @param kernal optional, a list defining the type of kernel used for computing the SPI atscales higher than one. Defaults to unshifted rectangular kernel.
#' @param ref.start optional, starting point of the reference period used for computing the index. Defaults to NULL, indicating that the first value in data will be used as starting point.
#' @param ref.end optional, ending  point  of  the  reference  period  used  for  computing  the index. Defaults to NULL, indicating that the last value in data will be used as ending point.
#' @param ... For more details please refer to the SPEI
#' @return A vector containing a monthly SPI at the selected scale
#' @author Christiana Photiadou (KNMI)
#' @references \url{http://www.ecad.eu/documents/atbd.pdf}
#' @references \url{https://cran.r-project.org/web/packages/SPEI/SPEI.pdf}
#' @importFrom SPEI spi
#' @importFrom stats ts
#' 
#' @export
climdex.spi3 <- function(ci, freq=c("monthly"), scale=3, distribution="Gamma", fit="ub-pwm", kernal=list(type="rectangular",shift=1), ref.start=NULL, ref.end=NULL){
  
  spiprec <- ci@data$prec
  spifactor <- ci@date.factors$monthly
  prec_sum <- as.numeric(tapply.fast(spiprec, spifactor, sum, na.rm=TRUE))
  
  ts.start <- c(as.numeric(format(ci@dates[1],format="%Y")),1)
  ts.end <- c(as.numeric(format(ci@dates[length(ci@dates)],format="%Y")),12)
  
  data.spi <- ts(prec_sum, frequency = 12,start = ts.start, end = ts.end)
  
  spi_col <- SPEI::spi(data.spi, scale=scale,ref.start=ref.start,ref.end=ref.end,
                       distribution=distribution,fit=fit,kernal=kernal,na.rm=TRUE)
  
  tmpvar <- (spi_col$fitted)
  tmpvar <- ifelse(tmpvar=="-Inf",NA,tmpvar)
  tmpvar <- ifelse(tmpvar=="Inf",NA,tmpvar)
  
  tmpvar <- ifelse(tmpvar=="NaNf",NA,tmpvar)
  tmpvar <- ifelse(tmpvar=="NaN",NA,tmpvar)
  
  x <- as.numeric(tmpvar)
  names(x) <- unique(spifactor)
  return(x)
}

#' Standardized Precipitation Index 6-mon
#' @description Adapted by ClimPACT2 and SPEI (arguments taken from here) libraries.
#' Given a timeseries of daily precipitation amounts [mm], the function calculates the Standardized Precipitation Index (SPI) as it is created and defined by the SPEI library.
#' This function computes the climdex index spi at multiple scales.
#' 
#' @param ci Object of type climdexInput (representing the daily precipitation [mm])
#' @param freq Time frequency to aggregate to. Allowed only monthly
#' @param scale an integer, representing the time scale at which the SPI will be computed. Default is 3
#' @param distribution name of the distribution function to be used for computing the SPI (default ’Gamma’)
#' @param fit name of the method used for computing the distribution function parameters (default 'ub-pwm')
#' @param kernel optional, a list defining the type of kernel used for computing the SPI atscales higher than one. Defaults to unshifted rectangular kernel.
#' @param ref.start optional, starting point of the reference period used for computing the index. Defaults to NULL, indicating that the first value in data will be used as starting point.
#' @param ref.end optional, ending  point  of  the  reference  period  used  for  computing  the index. Defaults to NULL, indicating that the last value in data will be used as ending point.
#' @param ... For more details please refer to the SPEI
#' @return A vector containing a monthly SPI at the selected scale
#' @author Christiana Photiadou (KNMI)
#' @references \url{http://www.ecad.eu/documents/atbd.pdf}
#' @references \url{https://cran.r-project.org/web/packages/SPEI/SPEI.pdf}
#' @importFrom SPEI spi
#' @importFrom stats ts
#' 
#' @export
climdex.spi6 <- function(ci, freq=c("monthly"), scale=6, distribution="Gamma", fit="ub-pwm", kernal=list(type="rectangular",shift=1), ref.start=NULL, ref.end=NULL){
  
  spiprec <- ci@data$prec
  spifactor <- ci@date.factors$monthly
  prec_sum <- as.numeric(tapply.fast(spiprec, spifactor, sum, na.rm=TRUE))
  
  ts.start <- c(as.numeric(format(ci@dates[1],format="%Y")),1)
  ts.end <- c(as.numeric(format(ci@dates[length(ci@dates)],format="%Y")),12)
  
  data.spi <- ts(prec_sum, frequency = 12, start = ts.start, end = ts.end)
  
  spi_col <- SPEI::spi(data.spi, scale=scale,ref.start=ref.start,ref.end=ref.end,
                       distribution=distribution,fit=fit,kernal=kernal,na.rm=TRUE)
  
  tmpvar <- (spi_col$fitted)
  tmpvar <- ifelse(tmpvar=="-Inf",NA,tmpvar)
  tmpvar <- ifelse(tmpvar=="Inf",NA,tmpvar)
  
  tmpvar <- ifelse(tmpvar=="NaNf",NA,tmpvar)
  tmpvar <- ifelse(tmpvar=="NaN",NA,tmpvar)
  
  x <- as.numeric(tmpvar)
  names(x) <- unique(spifactor)
  return(x)
}

#' Consecutive Summer Days
#' @description 
#' This function takes a climdexInput object as input and computes the climdex
#' index csu: the annual (or at different periods) count of consecutive summer days (TX >25C)
#' 
#' @param ci Object of type climdexInput. Here the daily maximum temperature.
#' @param freq Time frequency to aggregate to. Allowed only monthly, annual, halfyear, seasonal.
#' @param spells.can.span.years Default FALSE
#' @return A vector containing a timeseries of the number of consecutive summer days in a given period (freq).
#' @author Christiana Photiadou (KNMI)
#' @references \url{http://www.ecad.eu/documents/atbd.pdf}
#' 
#' @export
climdex.csu <- function(ci,freq=c("monthly","annual", "halfyear", "seasonal"),spells.can.span.years=FALSE) {
  stopifnot(!is.null(ci@data$tmax));
  return(spell.length.max(ci@data$tmax, ci@date.factors[[match.arg(freq)]], 25, ">", spells.can.span.years) * ci@namasks[[match.arg(freq)]]$tmax)
}

#' Consecutive Frost Days
#' @description 
#' This function takes a climdexInput object as input and computes the climdex
#' index cfd: the annual (or at different periods) count of consecutive frost days (TN < 0C)
#' 
#' @param ci Object of type climdexInput. Here the daily maximum temperature.
#' @param freq Time frequency to aggregate to. Allowed only monthly, annual, halfyear, seasonal.
#' @param spells.can.span.years Default FALSE
#' @return A vector containing a timeseries of the number of consecutive summer days in a given period (freq).
#' @author Christiana Photiadou (KNMI)
#' @references \url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
#' 
#' @export
climdex.cfd <- function(ci,freq=c("monthly","annual", "halfyear", "seasonal"),spells.can.span.years=FALSE) {
  stopifnot(!is.null(ci@data$tmin));
  return(spell.length.max(ci@data$tmin, ci@date.factors[[match.arg(freq)]], 0, "<", spells.can.span.years) * ci@namasks[[match.arg(freq)]]$tmin) }

#' Heating Degree Days
#' @description 
#' This function takes a climdexInput object as input and computes the climdex
#' index hd17: the annual (or at different periods) sum of heating degree days (17-tavg)
#' @param ci Object of type climdexInput. Here the daily maximum temperature.
#' @param freq Time frequency to aggregate to. Allowed only monthly, annual, halfyear, seasonal.
#' @param spells.can.span.years Default FALSE
#' @return A vector containing a timeseries of the number of consecutive summer days in a given period (freq).
#' @author Christiana Photiadou (KNMI)
#' @references \url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
#' 
#' @export
climdex.hd17 <- function(ci, freq=c("monthly","annual", "halfyear", "seasonal")) {
  stopifnot(!is.null(ci@data$tavg));
  return(tapply((17 -  ci@data$tavg), ci@date.factors[[match.arg(freq)]], sum)* ci@namasks[[match.arg(freq)]]$tavg)
}

#' Lengths of strings of TRUE (1 & 0) values
## -- introduced by C. Photiadou (KNMI), September 2015
#' Computes which days are above or below the baseline threshold.
#' 
#' This function computes which days are above or below baseline thresholds.
#' It is used to implement the compound indices.
#' It is based on the "percent.days.op.threshold"
#' 
#' @param temp Sequence of temperature values.
#' @param dates Sequence of associated dates.
#' @param jdays Sequence of associated days of year.
#' @param date.factor Factor to aggregate data using.
#' @param threshold.outside.base Sequence of thresholds to be used for data outside the base period.
#' @param base.thresholds Data structure containing sets of thresholds to be used inside the base period; see \link{climdexInput-class}.
#' @param base.range Date range (type PCICt) of the baseline period.
#' @param op Comparison operator to use.
#' @param max.missing.days Maximum number of NA values per time period.
#' @return A vector consisting of the mean fraction of days above or below the supplied set of thresholds.
#' @note If date.factor is omitted, daily series will be returned.
#' @seealso \link{climdexInput-class}.
#' @keywords ts climate
days.op.threshold <- function(temp, dates, jdays, date.factor, threshold.outside.base, base.thresholds, base.range, op='<') {
  f <- match.fun(op)
  dat <- f(temp, threshold.outside.base[jdays])
  
  inset <- dates >= base.range[1] & dates <= base.range[2]
  ## Don't use in-base thresholds with data shorter than two years; no years to replace with.
  if(sum(inset) > 0 && length(dates) >= 360 * 2) {
    jdays.base <- jdays[inset]
    years.base <- get.years(dates[inset])
    
    ## Get number of base years, subset temp data to base period only.
    temp.base <- temp[inset]
    years.base.range <- range(years.base)
    byrs <- (years.base.range[2] - years.base.range[1] + 1)
    
    ## Linearize thresholds, then compare them to the temperatures
    bdim <- dim(base.thresholds)
    dim(base.thresholds) <- c(bdim[1] * bdim[2], bdim[3])
    yday.byr.indices <- jdays.base + (years.base - get.years(base.range)[1]) * bdim[1]
    f.result <- f(rep(temp.base, byrs - 1), base.thresholds[yday.byr.indices,])
    dim(f.result) <- c(length(yday.byr.indices), bdim[3])
    
    ## Chop up data along the 2nd dim into a list; sum elements of the list
    dat[inset] <- rowSums(f.result, na.rm=TRUE) / (byrs - 1)
  }
  return(dat)
}

## Climate Compound Indices
## -- introduced by C. Photiadou (KNMI), September 2015
#' Compound Indices
#'
#' Cold-dry days
#'
#' This function computes the climdex index CD.
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index CD: the number of days where TG<25 & RR<25 (for wet days: days where precipitation is at least 1mm).
#' Note: this function doesnt use directly tavg but derives it from tmax & tmin (concistent with .ncdf)
#' @param ci Object of type climdexInput (representing the daily precipitation [mm] and the averaged daily temperature [C])
#' @return A vector containing an annual timeseries of precipitation in wet days.
#' @template generic_seealso_references
#'
#' @export
climdex.cd <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal"), precip.thresh="q25", precip.op="<", temp.thresh="q25", temp.op="<") {
  
  stopifnot(!is.null(ci@data$prec) && !is.null(ci@quantiles$prec) && !is.null(ci@data$tavg) && !is.null(ci@quantiles$tavg))
  daily.prec <- ci@data$prec
  daily.temp <- ci@data$tavg
  q.precip <- ci@quantiles$prec[[precip.thresh]]
  f.prec <- match.fun(precip.op)
  f.temp<- days.op.threshold(daily.temp, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], 
                             ci@quantiles$tavg$outbase[[temp.thresh]],
                             ci@quantiles$tavg$inbase[[temp.thresh]], ci@base.range, temp.op)
  #Convert from 1/0 to TRUE/FALSE
  logic.f.temp <- f.temp==1
  df.for.data <- data.frame(daily.prec, logic.f.temp, date.factor = ci@date.factors[[match.arg(freq)]])
  result <- lapply(split(df.for.data, df.for.data$date.factor), function(chunk) {
    sum(f.prec(chunk$daily.prec, q.precip) & chunk$logic.f.temp, na.rm=FALSE)
  })
  return(unlist(result))
}

#' Cold-wet days
#'
#' This function computes the climdex index CW.
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index CW: the number of days where TG<25 & RR>75 (for wet days: days where precipitation is at least 1mm).
#'
#' @param ci Object of type climdexInput (representing the daily precipitation [mm] and the averaged daily temperature [C])
#' @return A vector containing an annual timeseries of precipitation in wet days.
#' @template generic_seealso_references
#'
#' @export
climdex.cw <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal"), precip.thresh="q75", precip.op=">", temp.thresh="q25", temp.op="<") {
  
  stopifnot(!is.null(ci@data$prec) && !is.null(ci@quantiles$prec) && !is.null(ci@data$tavg) && !is.null(ci@quantiles$tavg))
  daily.prec <- ci@data$prec
  daily.temp <- ci@data$tavg
  q.precip <- ci@quantiles$prec[[precip.thresh]]
  f.prec <- match.fun(precip.op)
  f.temp<- days.op.threshold(daily.temp, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], 
                             ci@quantiles$tavg$outbase[[temp.thresh]],
                             ci@quantiles$tavg$inbase[[temp.thresh]], ci@base.range, temp.op)
  #Convert from 1/0 to TRUE/FALSE
  logic.f.temp <- f.temp==1
  df.for.data <- data.frame(daily.prec, logic.f.temp, date.factor = ci@date.factors[[match.arg(freq)]])
  result <- lapply(split(df.for.data, df.for.data$date.factor), function(chunk) {
    sum(f.prec(chunk$daily.prec, q.precip) & chunk$logic.f.temp, na.rm=FALSE)
  })
  return(unlist(result))
}

#' Warm-dry days
#'
#' This function computes the climdex index WD.
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index WD: the number of days where TG>75 & RR<25 (for wet days: days where precipitation is at least 1mm).
#'
#' @param ci Object of type climdexInput (representing the daily precipitation [mm] and the averaged daily temperature [C])
#' @return A vector containing an annual timeseries of precipitation in wet days.
#' @template generic_seealso_references
#'
#' @export
climdex.wd <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal"), precip.thresh="q25", precip.op="<", temp.thresh="q75", temp.op=">") {
  
  stopifnot(!is.null(ci@data$prec) && !is.null(ci@quantiles$prec) && !is.null(ci@data$tavg) && !is.null(ci@quantiles$tavg))
  daily.prec <- ci@data$prec
  daily.temp <- ci@data$tavg
  q.precip <- ci@quantiles$prec[[precip.thresh]]
  f.prec <- match.fun(precip.op)
  f.temp<- days.op.threshold(daily.temp, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], 
                             ci@quantiles$tavg$outbase[[temp.thresh]],
                             ci@quantiles$tavg$inbase[[temp.thresh]], ci@base.range, temp.op)
  #Convert from 1/0 to TRUE/FALSE
  logic.f.temp <- f.temp==1
  df.for.data <- data.frame(daily.prec, logic.f.temp, date.factor = ci@date.factors[[match.arg(freq)]])
  result <- lapply(split(df.for.data, df.for.data$date.factor), function(chunk) {
    sum(f.prec(chunk$daily.prec, q.precip) & chunk$logic.f.temp, na.rm=FALSE)
  })
  return(unlist(result))
}

#' Warm-wet days
#'
#' This function computes the climdex index WW
#'
#' This function takes a climdexInput object as input and computes the climdex
#' index WW: the number of days where TG>75 & RR>75 (for wet days: days where precipitation is at least 1mm).
#'
#' @param ci Object of type climdexInput (representing the daily precipitation [mm] and the averaged daily temperature [C])
#' @return A vector containing an annual timeseries of precipitation in wet days.
#' @template generic_seealso_references
#'
#' @export
climdex.ww <- function(ci, freq=c("monthly", "annual", "halfyear", "seasonal"), precip.thresh="q75", precip.op=">", temp.thresh="q75", temp.op=">") {
  
  stopifnot(!is.null(ci@data$prec) && !is.null(ci@quantiles$prec) && !is.null(ci@data$tavg) && !is.null(ci@quantiles$tavg))
  daily.prec <- ci@data$prec
  daily.temp <- ci@data$tavg
  q.precip <- ci@quantiles$prec[[precip.thresh]]
  f.prec <- match.fun(precip.op)
  f.temp<- days.op.threshold(daily.temp, ci@dates, ci@jdays, ci@date.factors[[match.arg(freq)]], 
                             ci@quantiles$tavg$outbase[[temp.thresh]],
                             ci@quantiles$tavg$inbase[[temp.thresh]], ci@base.range, temp.op)
  #Convert from 1/0 to TRUE/FALSE
  logic.f.temp <- f.temp==1
  df.for.data <- data.frame(daily.prec, logic.f.temp, date.factor = ci@date.factors[[match.arg(freq)]])
  result <- lapply(split(df.for.data, df.for.data$date.factor), function(chunk) {
    sum(f.prec(chunk$daily.prec, q.precip) & chunk$logic.f.temp, na.rm=FALSE)
  })
  return(unlist(result))
}

# ######################################################################################################################
# #' HUGLIN INDEX (only for climdex.pcic.ncdf)
#' Introduces by C.Photiadou (KNMI)
# #' This function is not ready yet. It is uses coefficient based on the latitude
# #' For this index I had to curry the cdx.funcs to be able to include the subset. Later I realised 
# #' I need also to include the latitude. This would be used in compute.indices.for.stripe together with get.lat 
# #' to retrieve subset & latitude 
# #' I didn't proceed with finisheing the eca.HI function. I thought to ask you first if its possible 
# #' to adapt compute.indices.for.stripe so it can include the currying and the latitude. Or if you had a better idea on this 
# #' please let me know. 
# 
# #' Function to Curry a cxd.funcs for subset (now at cur_sub)
# #' used only for Huglin Index
# curry_in_subset_for_huglin <- function(cdx.funcs, cur_sub){
#   cdx.names = names(cdx.funcs)
#   cdx.funcs <- lapply(cdx.names, function(function_name) {
#     f = cdx.funcs[[function_name]]
#     if(grepl('^hi', function_name)) {
#       return(functional::Curry(f, cur_sub = cur_sub))
#     } else {
#       return(f)
#     }
#   })
#   names(cdx.funcs) = cdx.names
#   return(cdx.funcs)
# }

# ### Get latitude function 
# get.lat <- function(open_file_list, variable.name.map) {
#   #var.name <- variable.name.map[[names(v.f.idx)[1]]]
#   y.dim <- ncdf4.helpers::nc.get.dim.for.axis(open_file_list[[1]], variable.name.map, "Y")
#   return(y.dim$vals)
# }

#' Huglin Index
#' 
#' This function computes the climdex index HI:
#' @param ci Object of type climdexInput (representing the daily mean and daily max temperature)
#' @param freq Time frequency to aggregate to. Allowed are only "annual"
#' @param unit. Allowed are only deg C.
#' @return A vector containing the HI
#' @author Rebekka Posselt (MeteoSwiss)
#' @references \url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
#' 
#' @export
climdex.HI <- function(ci,freq=c("annual"),cur_sub){
  
  tempavg <- ci@data$tavg
  tempmax <- ci@data$tmax  
  
  month.series <- get.months(ci@dates)
  year.series <- get.years(ci@dates)
  valid.months <- month.series >=4 & month.series <=9
  hi_coef <-  if (cur_sub <=40) {hi_coeff <- 1 
  }else if(cur_sub >40 & cur_sub <42) {hi_coef <- 1.02
  }else if(cur_sub >42 & cur_sub <44) {hi_coef <- 1.03
  }else if(cur_sub >44 & cur_sub <48) {hi_coef <- 1.04
  }else if(cur_sub >46 & cur_sub <48) {hi_coef <- 1.05
  }else if(cur_sub >48 & cur_sub <50) {hi_coef <- 1.06 
  }else if(cur_sub >=50){hi_coef <- 1}
  valid.sel<- year.series[valid.months]
  tempdata <- ((((tempavg -10) + (tempmax -10)) /2) * hi_coef)
  dat_final <- tempdata[valid.months]
  
  return(tapply(dat_final,valid.sel,sum))
}


#' Flexible GSL function
#' 
#' This function computes the growing season length (GSL) given the input,
#' which is allowed to vary considerably from the ETCCDI definitions.
#' 
#' This function is the function used to implement \code{\link{climdex.gsl}}.
#' It's designed to be flexible to allow for experimentation and testing of new
#' thresholds and methods.
#' 
#' If you need to use this code for experimentation in the southern hemisphere,
#' you'll need to rip off the climdex.gsl code to rotate the year around so
#' that July 1st is treated as January 1st.
#' 
#' See \code{\link{climdex.gsl}} for more information on what \code{gsl.mode}
#' does.
#' 
#' @param daily.mean.temp Timeseries of daily mean temperature (in degrees C),
#' padded out to end on a year boundary (ie: starts on January 1st of some
#' year, ends on December 31st).
#' @param date.factor Factor of the same length as daily.mean.temp that divides
#' the timeseries up into years of data.
#' @param dates The corresponding series of dates.
#' @param northern.hemisphere Whether the data is from the northern hemisphere.
#' @param min.length The minimum number of days above or below the threshold
#' temperature that defines the start or end of a growing season.
#' @param t.thresh The temperature threshold for being considered part of a
#' growing season (in degrees C).
#' @param gsl.mode The growing season length mode (ETCCDI mode is "GSL").
#' @return A vector containing the number of days in the growing season for
#' each year.
#' @seealso \code{\link{climdex.gsl}}, \code{\link{climdexInput.csv}}.
#' @keywords ts climate
#' @examples
#' library(PCICt)
#' 
#' ## Create a climdexInput object from some data already loaded in and
#' ## ready to go.
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
#' ## Create an annual timeseries of the growing season length in days.
#' gsl <- growing.season.length(ci@@data$tavg, ci@@date.factors$annual, ci@@dates,
#'                              ci@@northern.hemisphere, gsl.mode="GSL") * 
#'        ci@@namasks$annual$tavg
#' 
#' ## Print these out for testing purposes.
#' gsl
#' 
#' @export
growing.season.length <- function(daily.mean.temp, date.factor, dates, northern.hemisphere,
                                  min.length=6, t.thresh=5, gsl.mode=c("GSL", "GSL_first", "GSL_max", "GSL_sum")) {
  gsl.mode <- match.arg(gsl.mode)
  month.series <- get.months(dates)
  transition.month <- if(northern.hemisphere) 7 else 1
  if(gsl.mode == "GSL") {
    return(tapply.fast(1:length(daily.mean.temp), date.factor, function(idx) {
      temp.data <- daily.mean.temp[idx]
      ts.mid <- head(which(month.series[idx] == transition.month), n = 1)
      if(!length(ts.mid))
        return(NA)
      
      ts.len<- length(temp.data)
      gs.begin <- which(select.blocks.gt.length(temp.data[1:(ts.mid-1)] > t.thresh, min.length - 1))
      
      ## Growing season actually ends the day -before- the sequence of sketchy days
      gs.end <- which(select.blocks.gt.length(temp.data[ts.mid:ts.len] < t.thresh, min.length - 1)) - 1
      
      ## If no growing season start, 0 length; if no end, ends at end of year; otherwise, end - start + 1
      return(ifelse(length(gs.begin) == 0, 0, ifelse(length(gs.end) == 0, ts.len - gs.begin[1] + 1, gs.end[1] - gs.begin[1] + ts.mid)))
    }))
  } else {
    in.gsl <- !select.blocks.gt.length(!select.blocks.gt.length(daily.mean.temp >= t.thresh, min.length - 1), min.length - 1)
    warning("GSL_first, GSL_max, and GSL_sum are experimental alternative growing season length definitions. Use at your own risk.")
    
    innerfunc <- switch(gsl.mode, GSL_first=function(bl) { ifelse(any(bl > 0), (bl[bl > 0])[1], 0) }, GSL_max=max, GSL_sum=sum)
    return(tapply.fast(in.gsl, date.factor, function(ts) { block.lengths <- get.series.lengths.at.ends(ts); return(innerfunc(block.lengths)); }))
  }
}
