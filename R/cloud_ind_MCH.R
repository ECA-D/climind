# 1ST OUTER FUNCTION
#' Fraction of days which are within periods of at least n+1 consecutive day periods op threshold
#'
#' This function was originally written to calculate CCge4; the fraction of sunny/cloudy days which are within at least
#' 4 consecutive day periods. Sunny days were defined as daily average cloud covers < 3 okta and cloudy days
#' as daily average cloud covers >= 6 okta.
#'
#' @param ci Object of type climdexInput.
#' @param freq Time frequency/period to aggregate to.
#' @param thresh Threshold value
#' @param op Operator that compares cloud.okta with threshold.
#' @param n Maximum number of spell lenghts that should be skipped.
#'
#'
#' @return for each date.factor the fraction of days which are whithin
#' at least n+1 consecutive days which are op threshold.
#'
#' @examples
#' # example date.series
#' date.series <- seq(as.POSIXct("2017/01/01", tz="UTC"), as.POSIXct("2017/12/31", tz = "UTC"), by = "day")
#'
#' # example cloud.okta
#' sam <- rep(c(runif(300,0,3),runif(10,3,6), runif(500,6,8)), 1000)
#' cloud.okta <- sample(sam,length(date.series))
#'
#' # climdexInput object
#' ci <- climind::climdexInput.raw(cloud = cloud.okta,
#'                                      cloud.dates = PCICt::as.PCICt.POSIXct(date.series, cal="gregorian"))
#'
#' # CCge4 Index
#' CCge4.2 <- climdex.cloud.CCge4(ci = ci, freq = "seasonal", threshold = 3, op = "<", n = 3)
#' CCge4.6 <- climdex.cloud.CCge4(ci = ci, freq = "seasonal", threshold = 6, op = ">=", n = 3)
#'
#' @export
climdex.cloud.CCge4 <- function(ci, freq, threshold, op, n) {

  stopifnot(!is.null(ci@data$cloud))

  f <- match.fun(op)

  return(fraction.within.cons.op.threshold(cloud.bin = f(ci@data$cloud, threshold),
                                           date.factor = ci@date.factors[[pmatch(freq, names(ci@date.factors))]],
                                           n = n))
}

# 2ND OUTER FUNCTION
#' Quantile of lengths of spells op threshold
#'
#' This function was originally written to calculate the 98th quantile of the lenghts of sunny/cloudy
#' spells per season. However, any quantile can be chosen.
#' The center of the spell determines to which date.factor the spell is counted to and
#' theoretically, spells can last longer than the duration of a date.factor.
#'
#'
#' @param ci Object of type climdexInput.
#' @param thresh Threshold value
#' @param op Operator that compares cloud.okta with threshold.
#' @param q Quantile that should be calculated.
#'
#' @return for each date.factor the quantile of the spell lenghts op threshold
#'
#' @examples
#' # example date.series
#' date.series <- seq(as.POSIXct("2017/01/01", tz="UTC"), as.POSIXct("2017/12/31", tz = "UTC"), by = "day")
#'
#' # example cloud.okta
#' sam <- rep(c(runif(300,0,3),runif(10,3,6), runif(500,6,8)), 1000)
#' cloud.okta <- sample(sam,length(date.series))
#'
#' # climdexInput object
#' ci <- climind::climdexInput.raw(cloud = cloud.okta,
#'                                      cloud.dates = PCICt::as.PCICt.POSIXct(date.series, cal="gregorian"))
#'
#' # CC98 Index
#' CC98.2 <- climdex.cloud.CC98(ci = ci, threshold = 3, op = "<", q = 0.98)
#' CC98.6 <- climdex.cloud.CC98(ci = ci, threshold = 6, op = ">=", q = 0.98)
#'
#' @export
climdex.cloud.CC98 <- function(ci, threshold, op, q) {

  stopifnot(!is.null(ci@data$cloud))
  
  f <- match.fun(op)

  date.factor.seas <- as.factor(sapply(strsplit(x = as.character(ci@date.factors[["seasonal"]]),split = "-"),
                             function(x){return(x[2])}))

  return(spell.length.quantile(cloud.bin = f(ci@data$cloud, threshold),
                               date.factor = date.factor.seas,
                               q = q))
}

# 1st INNER FUNCTION
#' Fraction of days which are within spells of at least n+1 days
#'
#' This function calculates the fraction of days which are within spells with lengths of at least n+1 days.
#' It uses two functions, select.blocks.gt.length and number.days.op.threshold, which
#' were found in https://github.com/ECA-D/climind/blob/master/R/climdex.r
#'
#' @param cloud.bin Time series of binary data (counted are 1s)
#' @param date.factor Factor to aggregate by.
#' @param n Maximum number of spell lenghts that should be skipped.
#'
#' @return for each date.factor the fraction of days which are whithin
#' at least n+1 consecutive days
#'
fraction.within.cons.op.threshold <- function(cloud.bin, date.factor, n) {

  stopifnot(is.logical(cloud.bin), is.numeric(n), is.factor(date.factor))

  cloud.bin.new <- as.numeric(select.blocks.gt.length(d = cloud.bin, n = n))
  cloud.nr <- number.days.op.threshold(temp = cloud.bin.new, date.factor, threshold = 1, op = "==")

  return(cloud.nr/table(date.factor)) # convert to fraction per period length
}

# 2ND INNER FUNCTION
#' Quantile of lenghts of spells
#'
#' This function calculates the qth quantile of the spell lenghts.
#' The center of the spell determines to which date.factor the spell is counted to and
#' theoretically, spells can last longer than the duration of a date.factor.
#'
#' @param cloud.bin Time series of binary data (counted are 1s)
#' @param date.factor Factor to aggregate by.
#' @param q Quantile that should be calculated.
#'
#' @return for each date.factor, the qth quantile of the consecutive 1s
#'
spell.length.quantile <- function(cloud.bin, date.factor, q) {

  stopifnot(is.logical(cloud.bin), is.factor(date.factor), is.numeric(q), q >= 0, q <= 1.)

  cloud.bin <- as.numeric(cloud.bin)
  cloud.bin.rle <- rle(cloud.bin)
  cloud.bin.lengths <- cloud.bin.rle$lengths[which(cloud.bin.rle$values == 1)]
  cloud.bin.start <- cumsum(cloud.bin.rle$lengths)[which(cloud.bin.rle$values == 1)]-cloud.bin.rle$lengths[which(cloud.bin.rle$values == 1)]+1
  cloud.bin.end <- cumsum(cloud.bin.rle$lengths)[which(cloud.bin.rle$values == 1)]
  cloud.bin.center <- floor(cloud.bin.start+(cloud.bin.end-cloud.bin.start)/2)
  date.center <- date.factor[cloud.bin.center]

  return(tapply.fast(cloud.bin.lengths, date.center, quantile, probs = q))
}



