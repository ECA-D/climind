get.temp.var.quantiles <- function(filled.data, date.series, bs.date.series, 
                                   qtiles, bs.date.range, n, in.base=FALSE, 
                                   min.base.data.fraction.present=0.1) {
  base.data <- create.filled.series(filled.data, date.series, bs.date.series)
  if(in.base)
    return(list(outbase=zhang.running.qtile(base.data, dates.base=bs.date.series, 
                                            qtiles=qtiles, 
                                            bootstrap.range=bs.date.range, n=n, 
                                            min.fraction.present=min.base.data.fraction.present),
                inbase=zhang.running.qtile(base.data, dates.base=bs.date.series, 
                                           qtiles=qtiles, bootstrap.range=bs.date.range, 
                                           n=n, get.bootstrap.data=TRUE, 
                                           min.fraction.present=min.base.data.fraction.present)))
  else
    return(list(outbase=zhang.running.qtile(base.data, dates.base=bs.date.series, 
                                            qtiles=qtiles, bootstrap.range=bs.date.range, 
                                            n=n, min.fraction.present=min.base.data.fraction.present)))
}

get.prec.var.quantiles <- function(filled.prec, date.series, bs.date.range, qtiles=c(0.25, 0.75, 0.95, 0.99)) {
  wet.days <- !(is.na(filled.prec) | filled.prec < 1)
  inset <- date.series >= bs.date.range[1] & date.series <= bs.date.range[2] & !is.na(filled.prec) & wet.days
  pq <- quantile(filled.prec[inset], qtiles, type=8)
  names(pq) <- paste("q", qtiles * 100, sep="")
  return(pq)
}


#' Method for getting threshold quantiles for use in computing indices
#' 
#' This function creates threshold quantiles for use with climdexInput.raw
#' or climdexInput.csv.
#' 
#' This function takes input climate data at daily resolution, and produces as
#' output a set of threshold quantiles. This data structure can then be passed
#' to climdexInput.raw or climdexInput.csv.
#'
#' For more details on arguments, see \code{link{climdexInput.raw}}.
#'
#' @seealso \code{\link{climdex.pcic-package}}, \code{\link{climdexInput.raw}}.
#' @references \url{http://etccdi.pacificclimate.org/list_27_indices.shtml}
#' @keywords ts climate
#'
#' @param tmax Daily maximum temperature data.
#' @param tmin Daily minimum temperature data.
#' @param prec Daily total precipitation data.
#' @param tmax.dates Dates for the daily maximum temperature data.
#' @param tmin.dates Dates for the daily minimum temperature data.
#' @param prec.dates Dates for the daily total precipitation data.
#' @template climdexInput_common_params
#' @param quantiles Threshold quantiles for supplied variables.
#' @return A set of threshold quantiles
#' @note Units are assumed to be mm/day for precipitation and degrees Celsius
#' for temperature. No units conversion is performed internally.
#' 
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
#' quantiles <- get.outofbase.quantiles(ec.1018935.tmax$MAX_TEMP,
#' ec.1018935.tmin$MIN_TEMP, ec.1018935.prec$ONE_DAY_PRECIPITATION,
#' tmax.dates, tmin.dates, prec.dates, base.range=c(1971, 2000))
#'
#' @export
get.outofbase.quantiles <- function(tmax=NULL, tmin=NULL, tavg=NULL, prec=NULL, tmax.dates=NULL, 
                                    tmin.dates=NULL, tavg.dates=NULL, prec.dates=NULL, base.range=c(1961, 1990), 
                                    n=5, temp.qtiles=c(0.10, 0.25, 0.75, 0.90), 
                                    prec.qtiles=c(0.25, 0.75, 0.95, 0.99), 
                                    min.base.data.fraction.present=0.1) {
  days.threshold <- 359
  check.basic.argument.validity(tmax=tmax, tmax.dates=tmax.dates,
                                tmin=tmin, tmin.dates=tmin.dates,
                                tavg=tavg, tavg.dates=tavg.dates,
                                prec=prec, prec.dates=prec.dates,
                                base.range=base.range, n=n,
                                snow=NULL, snow.dates=NULL,              # All these extra variables can be set to NULL, as 
                                snow_new=NULL, snow_new.dates=NULL,      # the function does not need to be able to calculate
                                wind=NULL, wind.dates=NULL,              # their quantiles as they are never needed as quantiles
                                wind_gust=NULL, wind_gust.dates=NULL,    # for any index.
                                wind_dir=NULL, wind_dir.dates=NULL,
                                cloud=NULL, cloud.dates=NULL,
                                sun=NULL, sun.dates=NULL,
                                sun_rel=NULL, sun_rel.dates=NULL)
  
  d.list <- list(tmin.dates, tmax.dates, tavg.dates, prec.dates)
  all.dates <- do.call(c, d.list[!sapply(d.list, is.null)])
  last.day.of.year <- get.last.monthday.of.year(all.dates)
  cal <- attr(all.dates, "cal")
  
  bs.date.range <- as.PCICt(paste(base.range, c("01-01", last.day.of.year), sep="-"), cal=cal)
  new.date.range <- as.PCICt(paste(as.numeric(format(range(all.dates), "%Y", tz="GMT")), c("01-01", last.day.of.year), sep="-"), cal=cal)
  date.series <- seq(new.date.range[1], new.date.range[2], by="day")
  bs.date.series <- seq(bs.date.range[1], bs.date.range[2], by="day")
  
  quantiles <- list()
  
  if(!is.null(tmax)) {
    if(get.num.days.in.range(tmax.dates, bs.date.range) <= days.threshold)
      stop("There is less than a year of tmax data within the base period. Consider revising your base range and/or check your input data.")
    filled.tmax <- create.filled.series(tmax, trunc(tmax.dates, "days"), date.series)
    quantiles$tmax <- get.temp.var.quantiles(filled.tmax, date.series, bs.date.series, temp.qtiles, bs.date.range, n)
  } 
  
  if(!is.null(tmin)) {
    if(get.num.days.in.range(tmin.dates, bs.date.range) <= days.threshold)
      stop("There is less than a year of tmin data within the base period. Consider revising your base range and/or check your input data.")
    filled.tmin <- create.filled.series(tmin, trunc(tmin.dates, "days"), date.series)
    quantiles$tmin <- get.temp.var.quantiles(filled.tmin, date.series, bs.date.series, temp.qtiles, bs.date.range, n)
  }
  
  if(!is.null(tavg)) {
    if(get.num.days.in.range(tavg.dates, bs.date.range) <= days.threshold)
      stop("There is less than a year of tavg data within the base period. Consider revising your base range and/or check your input data.")
    filled.tavg <- create.filled.series(tavg, trunc(tavg.dates, "days"), date.series)
    quantiles$tavg <- get.temp.var.quantiles(filled.tavg, date.series, bs.date.series, temp.qtiles, bs.date.range, n)
  }
  
  if(!is.null(prec)) {
    if(get.num.days.in.range(prec.dates, bs.date.range) <= days.threshold)
      stop("There is less than a year of prec data within the base period. Consider revising your base range and/or check your input data.")
    filled.prec <- create.filled.series(prec, trunc(prec.dates, "days"), date.series)
    quantiles$prec <- get.prec.var.quantiles(filled.prec, date.series, bs.date.range, prec.qtiles)
  }
  return(quantiles)
}

## Returns an n-day running quantile for each day of data (dimensions c(dpy, q))
running.quantile <- function(data, n, q, dpy, min.fraction) {
  ret <- .Call("running_quantile_windowed", data, n, q, dpy, min.fraction, PACKAGE='climind')
  dim(ret) <- c(length(q), dpy)
  return(t(ret))
}

#' Climind quantile function
#' 
#' This function implements R's type=8 in a more efficient manner.
#' 
#' This is a reimplementation of R's type=8 created to improve the efficiency
#' of this package.
#' 
#' @param x Data to compute quantiles on.
#' @param q Quantiles to be computed.
#' @return A vector of the quantiles in question.
#' @seealso \code{\link{quantile}}
#' @keywords ts climate
#' @examples
#' 
#' ## Compute 10th, 50th, and 90th percentile of example data.
#' climdex.quantile(1:10, c(0.1, 0.5, 0.9))
#' 
#' @export
climdex.quantile <- function(x, q=c(0, 0.25, 0.5, 0.75, 1)) {
  return(.Call("c_quantile2", as.double(x), q, PACKAGE='climind'))
}