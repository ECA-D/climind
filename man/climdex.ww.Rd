% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indices.R
\name{climdex.ww}
\alias{climdex.ww}
\title{Warm-wet days}
\usage{
climdex.ww(ci, freq = c("monthly", "annual", "halfyear", "seasonal"),
  precip.thresh = "q75", precip.op = ">", temp.thresh = "q75",
  temp.op = ">")
}
\arguments{
\item{ci}{Object of type climdexInput (representing the daily precipitation [mm] and the averaged daily temperature [C])}

\item{freq}{Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".}
}
\value{
A vector containing an annual timeseries of precipitation in wet days.
}
\description{
This function computes the climdex index WW

This function takes a climdexInput object as input and computes the climdex
index WW: the number of days where TG>75 & RR>75 (for wet days: days where precipitation is at least 1mm).
}
