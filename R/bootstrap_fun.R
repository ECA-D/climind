## Get set of days for bootstrap use
get.bootstrap.set <- function(dates, bootstrap.range, win.size) {
  dpy <- ifelse(is.null(attr(dates, "dpy")), 365, attr(dates, "dpy"))
  return(dates >= bootstrap.range[1] & dates <= bootstrap.range[2] & (dpy == 360 | format(dates, format="%m-%d", tz="GMT") != "02-29"))
}

## Calculate a running quantile on the data set over the bootstrap range.
## If get.bootstrap.data is TRUE, use the Zhang boostrapping method described in Xuebin Zhang et al's 2005 paper, "Avoiding Inhomogeneity in Percentile-Based Indices of Temperature Extremes" J.Clim vol 18 pp.1647-1648, "Removing the 'jump'".
## Expects PCICt for all dates
zhang.running.qtile <- function(x, dates.base, qtiles, bootstrap.range, include.mask=NULL, n=5, get.bootstrap.data=FALSE, min.fraction.present=0.1) {
  inset <- get.bootstrap.set(dates.base, bootstrap.range, n)
  dpy <- ifelse(is.null(attr(dates.base, "dpy")), 365, attr(dates.base, "dpy"))
  nyears <- floor(sum(inset) / dpy)
  
  if(!is.null(include.mask))
    x[include.mask] <- NA
  
  bs.data <- x[inset]
  
  qdat <- NULL
  if(get.bootstrap.data) {
    d <- .Call("running_quantile_windowed_bootstrap", bs.data, n, qtiles, dpy, min.fraction.present, PACKAGE='climind')
    dim(d) <- c(dpy, nyears, nyears - 1, length(qtiles))
    qdat <- lapply(1:length(qtiles), function(x) { r <- d[,,,x, drop=FALSE]; dim(r) <- dim(r)[1:3]; r })
  } else {
    res <- running.quantile(bs.data, n, qtiles, dpy, min.fraction.present)
    qdat <- lapply(1:length(qtiles), function(x) { res[,x] })
  }
  names(qdat) <- paste("q", qtiles * 100, sep="")
  return(qdat)
}